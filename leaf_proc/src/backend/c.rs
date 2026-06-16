use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use syn::{DeriveInput, Meta, parse_macro_input};

#[allow(clippy::module_name_repetitions)]
pub fn c_compiler(item: TokenStream) -> TokenStream
{
	let input: DeriveInput = parse_macro_input!(item as DeriveInput);
	let name: syn::Ident = input.ident;

	let Some(attr) = input.attrs.iter().find(|x| return x.path().is_ident("name")) else {
		todo!("not attribute #[name = Compiler]")
	};

	let compiler_name = if let Meta::List(list) = &attr.meta {
		let Ok(ident): Result<syn::Ident, _> = list.parse_args() else {
			todo!("name is not an ident")
		};
		ident
	} else {
		todo!("attr.meta is not a list")
	};

	let expanded: TokenStream2 = quote! {
		impl From<#name> for crate::backend::c::compiler::CCompilers {
			fn from(value: #name) -> Self {
				Self::#compiler_name(value)
			}
		}
	};

	return expanded.into();
}
