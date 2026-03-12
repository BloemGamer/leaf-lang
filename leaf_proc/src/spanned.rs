use proc_macro::TokenStream;
use quote::quote;
use syn::{Data, DeriveInput, Fields, parse_macro_input};

/// Derive macro for the `Spanned` trait.
///
/// Automatically implements `fn span(&self) -> Span` based on the shape of
/// the type:
///
/// ## Structs
/// Must have a field named `span` of type `Span`. The implementation simply
/// returns `self.span`.
///
/// ```ignore
/// #[derive(Spanned)]
/// pub struct Foo {
///     pub span: Span,
///     pub name: String,
/// }
/// // expands to:
/// impl Spanned for Foo {
///     fn span(&self) -> Span { self.span }
/// }
/// ```
///
/// ## Enums
/// Every variant must either:
/// 1. Have a field named `span` (named or positional index 0 if tuple), **or**
/// 2. Contain a single inner value that itself implements `Spanned` (the macro
///    calls `.span()` on it).
///
/// The convention used throughout this codebase is: named-field variants carry
/// a `span` field, and newtype-like variants (e.g. `Foo(Bar)`) delegate to the
/// inner value's `Spanned` impl.
///
/// ```ignore
/// #[derive(Spanned)]
/// pub enum MyEnum {
///     Named { span: Span, value: i32 },
///     Delegated(SomeSpannedType),
/// }
/// ```
pub fn derive_spanned(input: TokenStream) -> TokenStream
{
	let vinput: DeriveInput = parse_macro_input!(input as DeriveInput);
	let name: &syn::Ident = &vinput.ident;
	let (impl_generics, ty_generics, where_clause) = vinput.generics.split_for_impl();

	let body: proc_macro2::TokenStream = match &vinput.data {
		// ── Struct ────────────────────────────────────────────────────────────
		Data::Struct(data_struct) => match &data_struct.fields {
			Fields::Named(fields) => {
				let has_span: bool = fields
					.named
					.iter()
					.any(|f| return f.ident.as_ref().map(|i| return i == "span").unwrap_or(false));

				if has_span {
					quote! { self.span }
				} else {
					return syn::Error::new_spanned(
						name,
						"#[derive(Spanned)] on a named-field struct requires a field named `span`",
					)
					.to_compile_error()
					.into();
				}
			}
			Fields::Unnamed(fields) => {
				if fields.unnamed.len() == 1 {
					quote! { self.0.span() }
				} else {
					return syn::Error::new_spanned(
						name,
						"#[derive(Spanned)] on a tuple struct requires exactly one field \
                             (which itself implements Spanned), or use a named struct with a `span` field",
					)
					.to_compile_error()
					.into();
				}
			}
			Fields::Unit => {
				return syn::Error::new_spanned(name, "#[derive(Spanned)] cannot be applied to a unit struct")
					.to_compile_error()
					.into();
			}
		},

		Data::Enum(data_enum) => {
			let arms = data_enum.variants.iter().map(|variant| {
				let variant_name = &variant.ident;

				match &variant.fields {
					Fields::Named(fields) => {
						let has_span = fields
							.named
							.iter()
							.any(|f| return f.ident.as_ref().map(|i| return i == "span").unwrap_or(false));

						if has_span {
							return quote! {
								#name::#variant_name { span, .. } => *span,
							};
						} else {
							let single: bool = fields.named.len() == 1;
							if single {
								let field_name = fields.named[0].ident.as_ref().expect("");
								return quote! {
									return #name::#variant_name { #field_name, .. } => #field_name.span(),
								};
							} else {
								let msg: String = format!(
									"variant `{}` has no `span` field and more than one named field; \
                                     cannot automatically derive Spanned",
									variant_name
								);
								return syn::Error::new_spanned(variant_name, msg).to_compile_error();
							}
						}
					}

					Fields::Unnamed(fields) => match fields.unnamed.len() {
						1 => {
							return quote! {
								#name::#variant_name(inner) => inner.span(),
							};
						}
						_ => {
							let msg: String = format!(
								"tuple variant `{}` must have exactly one field to derive Spanned",
								variant_name
							);
							return syn::Error::new_spanned(variant_name, msg).to_compile_error();
						}
					},

					Fields::Unit => {
						let msg: String = format!(
							"unit variant `{}` cannot be used with #[derive(Spanned)]; \
                             add a `span: Span` field or remove this variant from the type",
							variant_name
						);
						return syn::Error::new_spanned(variant_name, msg).to_compile_error();
					}
				}
			});

			quote! {
				match self {
					#(#arms)*
				}
			}
		}

		Data::Union(_) => {
			return syn::Error::new_spanned(name, "#[derive(Spanned)] is not supported on unions")
				.to_compile_error()
				.into();
		}
	};

	let expanded: proc_macro2::TokenStream = quote! {
		impl #impl_generics Spanned for #name #ty_generics #where_clause {
			fn span(&self) -> Span {
				#body
			}
		}
	};

	return expanded.into();
}
