use quote::{format_ident, quote};
use syn::{fold::Fold, spanned::Spanned, Attribute, Field};

enum FieldKind {
    Default,
    Composed,
}

struct NodeField {
    ty: syn::Type,
    kind: FieldKind,
    attrs: Vec<Attribute>,
}

enum NodeVariant {
    Tuple(Vec<NodeField>),
    Struct(Vec<(syn::Ident, NodeField)>),
}

enum NodeDesc {
    Struct(NodeVariant),
    Enum(Vec<(syn::Ident, NodeVariant)>),
}

struct NodeRepr {
    ident: syn::Ident,
    desc: NodeDesc,
}

impl FieldKind {
    fn from_attr(attr: &Attribute) -> Option<Self> {
        if matches!(attr.style, syn::AttrStyle::Inner(_)) {
            return None;
        }

        let syn::Meta::Path(path) = &attr.meta else {
            return None;
        };

        if path.is_ident("compose") {
            Some(Self::Composed)
        } else {
            None
        }
    }

    // TODO: consider "get_from_field_and_erase"
    fn from_field(field: &Field) -> Result<Self, syn::Error> {
        let mut result = None;
        for attr in &field.attrs {
            if let Some(kind) = Self::from_attr(attr) {
                if result.is_some() {
                    return Err(syn::Error::new(
                        attr.span(),
                        "Conflicting field attribute",
                    ));
                }
                result = Some(kind);
            }
        }
        Ok(result.unwrap_or(Self::Default))
    }
}

struct FoldStruct;

impl Fold for FoldStruct {
    fn fold_field(&mut self, field: Field) -> Field {
        Field {
            attrs: field
                .attrs
                .into_iter()
                .filter(|a| FieldKind::from_attr(a).is_none())
                .collect(),
            ..field
        }
    }
}

struct FoldContainer<'a> {
    ident: &'a syn::Ident,
    errors: Vec<syn::Error>,
    rewriter_arms: Vec<syn::Arm>,
    rewriter_fields: Vec<syn::FieldValue>,
}

impl<'a> Fold for FoldContainer<'a> {
    fn fold_item_struct(&mut self, i: syn::ItemStruct) -> syn::ItemStruct {
        let ident = self.ident.clone();

        let field_list = i.fields.iter().enumerate().map(|(i, f)| {
            f.ident.unwrap_or(format_ident!("{i}")).clone()
        });

        let i = syn::ItemStruct {
            ident: self.ident.clone(),
            fields: self.fold_fields(i.fields),
            ..i
        };

        let pattern = if i.tuple

        i
    }

    fn fold_item_enum(&mut self, i: syn::ItemEnum) -> syn::ItemEnum {
        syn::ItemEnum {
            ident: self.ident.clone(),
            variants: i.variants.into_iter().map(|v| self.fold_variant(v)).collect(),
            ..i
        }
    }

    fn fold_field(&mut self, field: Field) -> Field {
        let kind = match FieldKind::from_field(&field) {
            Ok(kind) => kind,
            Err(err) => { self.errors.push(err); return field; },
        };
        let field_ty = field.ty;
        let field_ty = match kind {
            FieldKind::Default => field_ty,
            FieldKind::Composed => syn::parse2(quote!(
                <#field_ty as lintree::TreeNode>::Container
            )).unwrap(),
        };

        let field_ident = field.ident.as_ref().unwrap();
        self.rewriter_fields.push(syn::parse2(match kind {
            FieldKind::Default => quote!(#field_ident),
            FieldKind::Composed => quote!(#field_ident: <_ as TreeNode>::into_container(field_ident)),
        }).unwrap());

        Field {
            attrs: field
                .attrs
                .into_iter()
                .filter(|a| FieldKind::from_attr(a).is_none())
                .collect(),
            ty: field_ty,
            ..field
        }
    }
}

fn join_compile_errors(errors: impl IntoIterator<Item=syn::Error>) -> syn::Error {
    let mut errors = errors.into_iter();
    let mut err =  errors.next().unwrap();
    err.extend(errors);
    err
}

#[proc_macro_attribute]
pub fn tree_node(
    _attr: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let input = syn::parse_macro_input!(input as syn::Item);

    if !matches!(input, syn::Item::Enum(..) | syn::Item::Struct(..)) {
        return syn::Error::new(
            input.span(),
            "Only enums and structs can be annotated with #[tree_node]",
        )
        .to_compile_error()
        .into();
    }

    let struct_ident = match &input {
        syn::Item::Enum(enum_) => enum_.ident.clone(),
        syn::Item::Struct(struct_) => struct_.ident.clone(),
        _ => unreachable!(),
    };

    let container_ident = format_ident!("{}Container", struct_ident);

    let mut fold_container = FoldContainer {
        ident: &container_ident,
        errors: Vec::new(),
        rewriter_arms: Vec::new(),
        rewriter_fields: Vec::new(),
    };

    let container = fold_container.fold_item(input.clone());
    if !fold_container.errors.is_empty() {
        return join_compile_errors(fold_container.errors).to_compile_error().into();
    }
    let struct_ = FoldStruct.fold_item(input);

    let (impl_generics, ty_generics, where_clause) = match &struct_ {
        syn::Item::Enum(enum_) => enum_.generics.split_for_impl(),
        syn::Item::Struct(struct_) => struct_.generics.split_for_impl(),
        _ => unreachable!(),
    };

    quote! {
        #struct_
        #container

        impl #impl_generics lintree::TreeNode for #struct_ident #ty_generics #where_clause {
            type Container = #container_ident<#ty_generics>;
        }

        unsafe impl #impl_generics lintree::RawNode for #container_ident #ty_generics #where_clause {
            fn size(&self) -> usize {
                std::mem::size_of::<Self>()
            }
        }
    }
    .into()
}
