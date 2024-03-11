use proc_macro2::TokenStream;
use quote::{format_ident, quote, ToTokens};

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum FieldKind {
    Default,
    Composed,
}

#[derive(Debug)]
struct NodeField {
    attrs: Vec<syn::Attribute>,
    vis: syn::Visibility,
    ty: syn::Type,
    ident: Option<syn::Ident>,
    kind: FieldKind,
    guaranteed_ident: syn::Ident,
}

impl NodeField {
    fn from_syn(input: syn::Field, index: usize) -> syn::Result<Self> {
        let mut kind = None;
        for attr in &input.attrs {
            if attr.path().is_ident("compose") {
                if kind.is_some() {
                    return Err(syn::Error::new_spanned(
                        attr,
                        "Multiple field kind attributes are not allowed",
                    ));
                }
                kind = Some(FieldKind::Composed);
            }
        }

        let attrs = input
            .attrs
            .into_iter()
            .filter(|attr| !attr.path().is_ident("compose"))
            .collect();

        let kind = kind.unwrap_or(FieldKind::Default);

        Ok(Self {
            attrs,
            vis: input.vis,
            ty: input.ty,
            ident: input.ident,
            kind,
            guaranteed_ident: format_ident!("__lintree_f{}", index),
        })
    }

    fn emit_base(&self) -> TokenStream {
        let Self {
            attrs,
            vis,
            ty,
            ident,
            ..
        } = self;

        match ident {
            None => quote!(#(#attrs)* #vis #ty),
            Some(ident) => quote!(#(#attrs)* #vis #ident: #ty),
        }
    }

    fn emit_container(&self) -> TokenStream {
        match self.kind {
            FieldKind::Default => self.emit_base(),
            FieldKind::Composed => {
                let Self {
                    attrs,
                    vis,
                    ty,
                    ident,
                    ..
                } = self;

                match ident {
                    None => quote! {
                        #(#attrs)* #vis <#ty as lintree::TreeNode>::Container
                    },
                    Some(ident) => quote! {
                        #(#attrs)* #vis #ident: <#ty as lintree::TreeNode>::Container
                    },
                }
            }
        }
    }
}

#[derive(Debug)]
enum NodeFields {
    Named(Vec<NodeField>),
    Unnamed(Vec<NodeField>),
    Unit,
}

impl NodeFields {
    fn from_syn(input: syn::Fields) -> syn::Result<Self> {
        Ok(match input {
            syn::Fields::Unit => Self::Unit,
            syn::Fields::Named(fields) => {
                let fields = fields
                    .named
                    .into_iter()
                    .enumerate()
                    .map(|(i, f)| NodeField::from_syn(f, i))
                    .collect::<syn::Result<_>>()?;
                Self::Named(fields)
            }
            syn::Fields::Unnamed(fields) => {
                let fields = fields
                    .unnamed
                    .into_iter()
                    .enumerate()
                    .map(|(i, f)| NodeField::from_syn(f, i))
                    .collect::<syn::Result<_>>()?;
                Self::Unnamed(fields)
            }
        })
    }

    fn emit_base(&self) -> TokenStream {
        match self {
            Self::Named(fields) => {
                let fields = fields.iter().map(NodeField::emit_base);
                quote!({ #(#fields),* })
            }
            Self::Unnamed(fields) => {
                let fields = fields.iter().map(NodeField::emit_base);
                quote!(( #(#fields),* ))
            }
            Self::Unit => quote!(),
        }
    }

    fn emit_container(&self) -> TokenStream {
        match self {
            Self::Named(fields) => {
                let fields = fields.iter().map(NodeField::emit_container);
                quote!({ #(#fields),* })
            }
            Self::Unnamed(fields) => {
                let fields = fields.iter().map(NodeField::emit_container);
                quote!(( #(#fields),* ))
            }
            Self::Unit => quote!(),
        }
    }

    fn emit_extra_size(&self) -> TokenStream {
        let fields = match self {
            Self::Named(fields) => fields,
            Self::Unnamed(fields) => fields,
            Self::Unit => return quote!(0),
        };

        let extra_size = fields
            .iter()
            .filter(|f| f.kind == FieldKind::Composed)
            .map(|field| {
                let ident = &field.guaranteed_ident;
                quote! {
                    lintree::RawNode::extra_size(#ident)
                }
            });

        quote!(0usize #(+ #extra_size)*)
    }

    fn emit_pattern_arm(&self) -> TokenStream {
        match self {
            Self::Named(fields) => {
                let names = fields.iter().map(|field| field.ident.as_ref().unwrap());
                let aliases = fields.iter().map(|field| &field.guaranteed_ident);
                quote!({ #(#names: #aliases),* })
            }
            Self::Unnamed(fields) => {
                let fields = fields.iter().map(|field| {
                    let ident = &field.guaranteed_ident;
                    quote!(#ident)
                });
                quote!((#(#fields),*))
            }
            Self::Unit => quote!(),
        }
    }

    fn emit_constructor(&self, args: impl IntoIterator<Item = TokenStream>) -> TokenStream {
        let args = args.into_iter();
        match self {
            Self::Named(fields) => {
                let fields = fields.iter().map(|field| {
                    let ident = field.ident.as_ref().unwrap();
                    quote!(#ident)
                });
                quote!({ #(#fields: #args),* })
            }
            Self::Unnamed(..) => {
                quote!((#(#args),*))
            }
            Self::Unit => quote!(),
        }
    }

    fn iter(&self) -> impl Iterator<Item = &NodeField> {
        match self {
            Self::Named(fields) => fields.iter(),
            Self::Unnamed(fields) => fields.iter(),
            Self::Unit => [].iter(),
        }
    }

    fn emit_into_container_fields<'a>(&'a self) -> impl Iterator<Item = TokenStream> + 'a {
        self.iter().map(|field| {
            if field.kind == FieldKind::Default {
                field.guaranteed_ident.to_token_stream()
            } else {
                let ident = &field.guaranteed_ident;
                quote!(lintree::TreeNode::into_container(#ident, _discriminator))
            }
        })
    }
}

#[derive(Debug)]
struct NodeStruct {
    attrs: Vec<syn::Attribute>,
    vis: syn::Visibility,
    ident: syn::Ident,
    generics: syn::Generics,
    fields: NodeFields,
    container_ident: syn::Ident,
}

impl NodeStruct {
    fn from_syn(input: syn::ItemStruct) -> syn::Result<Self> {
        let container_ident = format_ident!("{}Container", input.ident);
        Ok(Self {
            attrs: input.attrs,
            vis: input.vis,
            ident: input.ident,
            generics: input.generics,
            fields: NodeFields::from_syn(input.fields)?,
            container_ident,
        })
    }

    fn emit_base(&self) -> TokenStream {
        let Self {
            attrs,
            vis,
            ident,
            generics,
            fields,
            ..
        } = self;

        let fields = fields.emit_base();

        quote!(#(#attrs)* #vis struct #ident #generics #fields)
    }

    fn emit_container(&self) -> TokenStream {
        let Self {
            attrs,
            vis,
            generics,
            fields,
            container_ident,
            ..
        } = self;

        let fields = fields.emit_container();

        quote!(#(#attrs)* #vis struct #container_ident #generics #fields)
    }

    fn emit_container_raw_node_impl(&self) -> TokenStream {
        let Self { ident, fields, .. } = self;

        let extra_size = fields.emit_extra_size();
        let (impl_generics, ty_generics, where_clause) = self.generics.split_for_impl();

        quote! {
            unsafe impl #impl_generics lintree::RawNode for #ident #ty_generics #where_clause {
                fn extra_size(&self) -> usize {
                    #extra_size
                }
            }
        }
    }

    fn emit_tree_node_impl(&self) -> TokenStream {
        let Self {
            ident,
            container_ident,
            fields,
            generics,
            ..
        } = self;

        let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();
        let fields_pat = fields.emit_pattern_arm();
        let constructor = fields.emit_constructor(fields.iter().map(|f| match f.kind {
            FieldKind::Default => f.guaranteed_ident.to_token_stream(),
            FieldKind::Composed => {
                let ident = &f.guaranteed_ident;
                quote!(lintree::TreeNode::into_container(#ident))
            }
        }));

        quote! {
            impl #impl_generics lintree::TreeNode for #ident #ty_generics #where_clause {
                type Container = #container_ident #ty_generics;

                fn into_container(self, _discriminator: lintree::TreeDiscriminator) -> Self::Container {
                    let Self #fields_pat = self;
                    #container_ident #constructor
                }
            }
        }
    }

    fn emit_view(&self) -> TokenStream {
        todo!()
    }
}

#[derive(Debug)]
struct NodeVariant {
    attrs: Vec<syn::Attribute>,
    ident: syn::Ident,
    fields: NodeFields,
    discriminant: Option<syn::Expr>,
}

impl NodeVariant {
    fn from_syn(input: syn::Variant) -> syn::Result<Self> {
        Ok(Self {
            attrs: input.attrs,
            ident: input.ident,
            fields: NodeFields::from_syn(input.fields)?,
            discriminant: input.discriminant.map(|(_, expr)| expr),
        })
    }

    fn emit_base(&self) -> TokenStream {
        let Self {
            attrs,
            ident,
            fields,
            discriminant,
            ..
        } = self;

        let fields = fields.emit_base();

        quote!(#(#attrs)* #ident #fields #discriminant)
    }

    fn emit_container(&self) -> TokenStream {
        let Self {
            attrs,
            ident,
            fields,
            discriminant,
        } = self;

        let fields = fields.emit_container();

        quote!(#(#attrs)* #ident #fields #discriminant)
    }
}

#[derive(Debug)]
struct NodeEnum {
    attrs: Vec<syn::Attribute>,
    vis: syn::Visibility,
    ident: syn::Ident,
    generics: syn::Generics,
    variants: Vec<NodeVariant>,
    container_ident: syn::Ident,
    view_ident: syn::Ident,
    view_mut_ident: syn::Ident,
    view_generics: syn::Generics,
}

impl NodeEnum {
    fn from_syn(input: syn::ItemEnum) -> syn::Result<Self> {
        let container_ident = format_ident!("{}Container", input.ident);
        let view_ident = format_ident!("{}View", input.ident);
        let view_mut_ident = format_ident!("{}ViewMut", input.ident);
        let mut view_generics = input.generics.clone();
        view_generics.params.insert(0, syn::parse2(quote!('__lintree_t)).unwrap());
        Ok(Self {
            attrs: input.attrs,
            vis: input.vis,
            ident: input.ident,
            generics: input.generics,
            variants: input
                .variants
                .into_iter()
                .map(NodeVariant::from_syn)
                .collect::<syn::Result<_>>()?,
            container_ident,
            view_ident,
            view_mut_ident,
            view_generics,
        })
    }

    fn emit_base(&self) -> TokenStream {
        let Self {
            attrs,
            vis,
            ident,
            generics,
            variants,
            ..
        } = self;

        let variants = variants.iter().map(NodeVariant::emit_base);

        quote!(#(#attrs)* #vis enum #ident #generics { #(#variants),* })
    }

    fn emit_container(&self) -> TokenStream {
        let Self {
            attrs,
            vis,
            generics,
            variants,
            container_ident,
            ..
        } = self;

        let variants = variants.iter().map(NodeVariant::emit_container);

        quote!(#(#attrs)* #vis enum #container_ident #generics { #(#variants),* })
    }

    fn emit_container_raw_node_impl(&self) -> TokenStream {
        let arms = self.variants.iter().map(|variant| {
            let var_ident = &variant.ident;
            let pattern = variant.fields.emit_pattern_arm();
            let extra_size = variant.fields.emit_extra_size();
            quote! {
                Self::#var_ident #pattern => #extra_size
            }
        });

        let ident = &self.container_ident;
        let (impl_generics, ty_generics, where_clause) = self.generics.split_for_impl();

        quote! {
            unsafe impl #impl_generics lintree::RawNode for #ident #ty_generics #where_clause {
                fn extra_size(&self) -> usize {
                    match self {
                        #(#arms),*
                    }
                }
            }
        }
    }

    fn emit_tree_node_impl(&self) -> TokenStream {
        let Self {
            ident,
            view_ident,
            view_mut_ident,
            generics,
            view_generics,
            variants,
            ..
        } = self;
        let container_ident = format_ident!("{}Container", ident);
        let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

        let arms = variants.iter().map(|variant| {
            let variant_ident = &variant.ident;
            let pattern = variant.fields.emit_pattern_arm();
            let constructor = variant
                .fields
                .emit_constructor(variant.fields.emit_into_container_fields());
            
            quote! {
                #ident::#variant_ident #pattern => {
                    #container_ident::#variant_ident #constructor
                }
            }
        });

        quote! {
            impl #impl_generics lintree::TreeNode for #ident #ty_generics #where_clause {
                type Container = #container_ident #ty_generics;
                type View<'__lintree_t> = #view_ident #view_generics
                    where Self: '__lintree_t, Self::Container: '__lintree_t;
                type ViewMut<'__lintree_t> = #view_mut_ident #view_generics
                    where Self: '__lintree_t, Self::Container: '__lintree_t;

                fn into_container(self, _discriminator: lintree::TreeDiscriminator) -> Self::Container {
                    match self {
                        #(#arms),*
                    }
                }
            }
        }
    }

    fn emit_view(&self) -> TokenStream {
        let Self {
            ident,
            vis,
            view_ident,
            view_mut_ident,
            generics,
            view_generics,
            ..
        } = self;
        let base = quote!(#ident<#generics>);
        let (impl_generics, ty_generics, where_generics) = view_generics.split_for_impl();

        quote! {
            #vis struct #view_ident #view_generics {
                raw: lintree::RawTreeView<'__lintree_t, #base>,
                data: &'__lintree_t <#base as lintree::TreeNode>::Container,
            }

            #vis struct #view_mut_ident #view_generics {
                raw: lintree::RawTreeViewMut<'__lintree_t, #base>,
                data: &'__lintree_t <#base as lintree::TreeNode>::Container,
            }

            impl #impl_generics lintree::NodeView<'__lintree_t> for #view_ident #ty_generics #where_generics {
                type Node = #base;

                fn from_tr(raw_tree: lintree::RawTreeView<'__lintree_t, Self::Node>, tr: lintree::Tr<Self::Node>) -> Self {
                    let data = raw_tree.get(tr);
                    Self {
                        raw: raw_tree,
                        data
                    }
                }
            }

            impl #impl_generics lintree::NodeViewMut<'__lintree_t> for #view_mut_ident #ty_generics #where_generics {
                type Node = #base;

                fn from_tr(raw_tree: lintree::RawTreeViewMut<'__lintree_t, Self::Node>, tr: lintree::Tr<Self::Node>) -> Self {
                    let data = raw_tree.get_mut(tr);
                    Self {
                        raw: raw_tree,
                        data
                    }
                }
            }

        }
    }
}

#[derive(Debug)]
enum NodeItem {
    Struct(NodeStruct),
    Enum(NodeEnum),
}

impl NodeItem {
    fn from_syn(input: syn::Item) -> syn::Result<Self> {
        match input {
            syn::Item::Struct(item) => Ok(NodeItem::Struct(NodeStruct::from_syn(item)?)),
            syn::Item::Enum(item) => Ok(NodeItem::Enum(NodeEnum::from_syn(item)?)),
            _ => Err(syn::Error::new_spanned(
                input,
                "#[tree_node] can only be applied to enums or structs",
            )),
        }
    }

    fn emit_base(&self) -> TokenStream {
        match self {
            Self::Struct(node) => node.emit_base(),
            Self::Enum(node) => node.emit_base(),
        }
    }

    fn emit_container(&self) -> TokenStream {
        match self {
            Self::Struct(node) => node.emit_container(),
            Self::Enum(node) => node.emit_container(),
        }
    }

    fn emit_container_raw_node_impl(&self) -> TokenStream {
        match self {
            Self::Struct(node) => node.emit_container_raw_node_impl(),
            Self::Enum(node) => node.emit_container_raw_node_impl(),
        }
    }

    fn emit_tree_node_impl(&self) -> TokenStream {
        match self {
            Self::Struct(node) => node.emit_tree_node_impl(),
            Self::Enum(node) => node.emit_tree_node_impl(),
        }
    }

    fn emit_view(&self) -> TokenStream {
        match self {
            Self::Struct(node) => node.emit_view(),
            Self::Enum(node) => node.emit_view(),
        }
    }
}

#[proc_macro_attribute]
pub fn tree_node(
    _attr: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let input = syn::parse_macro_input!(input as syn::Item);

    let repr = match NodeItem::from_syn(input) {
        Ok(repr) => repr,
        Err(err) => return err.to_compile_error().into(),
    };

    let base = repr.emit_base();
    let container = repr.emit_container();
    let raw_node_impl = repr.emit_container_raw_node_impl();
    let tree_node_impl = repr.emit_tree_node_impl();
    let view = repr.emit_view();

    let expanded = quote! {
        #base
        #container
        #raw_node_impl
        #tree_node_impl
        #view
    };

    expanded.into()
}
