#[proc_macro_derive(TreeNode)]
pub fn derive_tree_node(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = syn::parse_macro_input!(input as syn::DeriveInput);
    let name = &input.ident;
    let generics = &input.generics;
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    let expanded = quote::quote! {
        unsafe impl #impl_generics lintree::RawNode for #name #ty_generics #where_clause {
            fn size(&self) -> usize {
                std::mem::size_of::<Self>()
            }
        }
    };

    expanded.into()
}

