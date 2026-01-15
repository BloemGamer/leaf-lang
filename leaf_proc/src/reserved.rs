use proc_macro::TokenStream;
use quote::quote;
use syn::{Attribute, ItemEnum, Lit, Variant, parse_macro_input};

enum ReservedState
{
	None,
	Marker,
	Message(String),
}

pub fn reserved_tokens(_attr: TokenStream, item: TokenStream) -> TokenStream
{
	let input: ItemEnum = parse_macro_input!(item as ItemEnum);

	return expand_reserved_in_enum(&input).into();
}

fn expand_reserved_in_enum(input: &ItemEnum) -> proc_macro2::TokenStream
{
	let enum_name: &syn::Ident = &input.ident;
	let vis: &syn::Visibility = &input.vis;
	let attrs: &Vec<syn::Attribute> = &input.attrs;
	let generics: &syn::Generics = &input.generics;

	let match_arms = input.variants.iter().map(|variant| {
		let name = &variant.ident;
		let state = reserved_state(&variant.attrs);

		let pattern = match &variant.fields {
			syn::Fields::Unit => quote! { Self::#name },
			syn::Fields::Unnamed(_) => quote! { Self::#name(..) },
			syn::Fields::Named(_) => quote! { Self::#name { .. } },
		};

		return match state {
			ReservedState::None => quote! { #pattern => Ok(()) },
			ReservedState::Marker => {
				quote! { #pattern => Err(crate::lexer::ReservedError { token: self.clone() }) }
			}
			ReservedState::Message(_msg) => {
				unimplemented!("reserved with a message is for now not a feature")
			}
		};
	});

	let variants: Vec<Variant> = input
		.variants
		.iter()
		.map(|variant| {
			let mut new_variant = variant.clone();
			new_variant
				.attrs
				.retain(|attr| return !attr.path().is_ident("reserved"));
			return new_variant;
		})
		.collect();

	return quote! {
		#(#attrs)*
		#vis enum #enum_name #generics {
			#(#variants),*
		}

		impl #generics #enum_name #generics {
			pub fn check_reserved(&self) -> Result<(), crate::lexer::ReservedError> {
				match self {
					#(#match_arms),*
				}
			}

		}
	};
}

fn reserved_state(attrs: &[Attribute]) -> ReservedState
{
	for attr in attrs {
		if attr.path().is_ident("reserved") {
			match attr.parse_args::<Lit>() {
				Ok(Lit::Str(lit)) => {
					let s = lit.value();
					if s.is_empty() {
						return ReservedState::Marker;
					} else {
						return ReservedState::Message(s);
					}
				}
				Ok(_) | Err(_) => {
					return ReservedState::Marker;
				}
			}
		}
	}

	return ReservedState::None;
}
