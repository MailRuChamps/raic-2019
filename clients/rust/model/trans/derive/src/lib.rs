#![recursion_limit = "256"]
extern crate proc_macro;

use quote::quote;

use proc_macro2::TokenStream;

#[proc_macro_derive(Trans, attributes(trans))]
pub fn derive_trans(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input: TokenStream = input.into();
    let result: TokenStream = {
        let ast: syn::DeriveInput = syn::parse_str(&input.to_string()).unwrap();
        let input_type = &ast.ident;
        let mut magic: Option<syn::Expr> = None;
        for attr in &ast.attrs {
            if let Ok(syn::Meta::List(syn::MetaList {
                path: ref meta_path,
                nested: ref nested,
                ..
            })) = attr.parse_meta()
            {
                if meta_path.is_ident("trans") {
                    for inner in nested {
                        if let syn::NestedMeta::Meta(syn::Meta::NameValue(syn::MetaNameValue {
                            path: ref meta_path,
                            lit: syn::Lit::Str(ref lit),
                            ..
                        })) = *inner
                        {
                            if meta_path.is_ident("magic") {
                                magic = Some(syn::parse_str(&lit.value()).unwrap());
                            }
                        }
                    }
                }
            }
        }
        let (magic_write, magic_read) = match magic {
            Some(magic) => (
                quote! {
                    trans::Trans::write_to(&#magic, &mut writer)?;
                },
                quote! {
                    assert_eq!(<i32 as trans::Trans>::read_from(&mut reader)?, #magic);
                },
            ),
            None => (quote! {}, quote! {}),
        };
        match ast.data {
            syn::Data::Struct(syn::DataStruct { ref fields, .. }) => {
                let field_tys: Vec<_> = fields.iter().map(|field| &field.ty).collect();
                let field_tys = &field_tys;
                let field_names: Vec<_> = fields
                    .iter()
                    .enumerate()
                    .map(|(index, field)| {
                        field.ident.clone().map_or_else(
                            || syn::Member::Unnamed(index.into()),
                            |ident| syn::Member::Named(ident),
                        )
                    })
                    .collect();
                let field_names = &field_names;
                let mut generics = ast.generics.clone();
                let extra_where_clauses = quote! {
                    where #(#field_tys: trans::Trans,)*
                };
                let extra_where_clauses: syn::WhereClause =
                    syn::parse_str(&extra_where_clauses.to_string()).unwrap();
                generics
                    .make_where_clause()
                    .predicates
                    .extend(extra_where_clauses.predicates);
                let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();
                let expanded = quote! {
                    impl #impl_generics trans::Trans for #input_type #ty_generics #where_clause {
                        fn write_to(&self, mut writer: impl std::io::Write) -> std::io::Result<()> {
                            #magic_write
                            #(trans::Trans::write_to(&self.#field_names, &mut writer)?;)*
                            Ok(())
                        }
                        fn read_from(mut reader: impl std::io::Read) -> std::io::Result<Self> {
                            #magic_read
                            Ok(Self {
                                #(#field_names: trans::Trans::read_from(&mut reader)?),*
                            })
                        }
                    }
                };
                expanded.into()
            }
            syn::Data::Enum(syn::DataEnum { ref variants, .. }) => {
                let mut generics = ast.generics.clone();
                let all_field_tys = variants
                    .iter()
                    .map(|variant| variant.fields.iter().map(|field| &field.ty))
                    .flatten();
                let extra_where_clauses = quote! {
                    where #(#all_field_tys: trans::Trans + 'static,)*
                };
                let extra_where_clauses: syn::WhereClause =
                    syn::parse_str(&extra_where_clauses.to_string()).unwrap();
                generics
                    .make_where_clause()
                    .predicates
                    .extend(extra_where_clauses.predicates);
                let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();
                let variant_writes = variants.iter().enumerate().map(|(discriminant, variant)| {
                    let discriminant = discriminant as i32;
                    let variant_name = &variant.ident;
                    let field_names: Vec<_> = variant
                        .fields
                        .iter()
                        .map(|field| field.ident.as_ref().unwrap())
                        .collect();
                    let field_names = &field_names;
                    let field_names_copy = field_names;
                    quote! {
                        #input_type::#variant_name { #(#field_names,)* } => {
                            trans::Trans::write_to(&#discriminant, &mut writer)?;
                            #(trans::Trans::write_to(#field_names_copy, &mut writer)?;)*
                        }
                    }
                });
                let variant_reads = variants.iter().enumerate().map(|(discriminant, variant)| {
                    let discriminant = discriminant as i32;
                    let variant_name = &variant.ident;
                    let field_names = variant
                        .fields
                        .iter()
                        .map(|field| field.ident.as_ref().unwrap());
                    quote! {
                        #discriminant => #input_type::#variant_name {
                            #(#field_names: trans::Trans::read_from(&mut reader)?,)*
                        },
                    }
                });
                let expanded = quote! {
                    impl #impl_generics trans::Trans for #input_type #ty_generics #where_clause {
                        fn write_to(&self, mut writer: impl std::io::Write) -> std::io::Result<()> {
                            match self {
                                #(#variant_writes)*
                            }
                            Ok(())
                        }
                        fn read_from(mut reader: impl std::io::Read) -> std::io::Result<Self> {
                            Ok(match <i32 as trans::Trans>::read_from(&mut reader)? {
                                #(#variant_reads)*
                                discriminant => panic!("Unexpected discriminant {:?}", discriminant),
                            })
                        }
                    }
                };
                expanded.into()
            }
            syn::Data::Union(_) => panic!("Unions not supported"),
        }
    };
    result.into()
}
