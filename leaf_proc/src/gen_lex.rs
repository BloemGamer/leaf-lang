use proc_macro::TokenStream;
use quote::quote;
use syn::{Attribute, Data, DeriveInput, Error, Result, parse_macro_input, spanned::Spanned};

/// Generates lexer helper methods from `TokenKind` enum attributes.
///
/// Supports:
/// - `#[keyword("text")]` - Generates keyword matching
/// - `#[operator("op")]` - Generates operator lexing (single or multi-char)
/// - `#[simple_token("x")]` - Generates simple token matching (single char only)
/// - `#[reserved]` - Marks tokens as reserved (generates `check_reserved`)
///
/// # Example
/// ```rustdoc
/// #[derive(Debug, Clone, PartialEq)]
/// #[generate_lexer]
/// pub enum TokenKind {
///     #[keyword("if")]
///     If,
///
///     #[operator("+")]
///     Plus,
///
///     #[operator("+=")]
///     PlusEquals,
///
///     #[operator(".")]
///     Dot,
///
///     #[operator("..")]
///     DotDot,
///
///     #[simple_token("(")]
///     LeftParen,
///
///     #[reserved]
///     #[keyword("async")]
///     Async,
///
///     Identifier(String),
/// }
/// ```
pub fn generate_lexer(_attr: TokenStream, item: TokenStream) -> TokenStream
{
	let input: DeriveInput = parse_macro_input!(item as DeriveInput);

	return match generate_lexer_impl(&input) {
		Ok(tokens) => tokens.into(),
		Err(err) => err.to_compile_error().into(),
	};
}

fn generate_lexer_impl(input: &DeriveInput) -> Result<proc_macro2::TokenStream>
{
	let enum_data: &syn::DataEnum = match &input.data {
		Data::Enum(data) => data,
		_ => return Err(Error::new(input.span(), "generate_lexer can only be used on enums")),
	};

	let enum_name: &syn::Ident = &input.ident;
	let enum_vis: &syn::Visibility = &input.vis;
	let enum_attrs: &Vec<Attribute> = &input.attrs;

	let mut keywords: Vec<(String, syn::Ident)> = Vec::new();
	let mut operators: Vec<(syn::Ident, OperatorInfo)> = Vec::new();
	let mut simple_tokens: Vec<(String, syn::Ident)> = Vec::new();
	let mut reserved_variants: Vec<&syn::Ident> = Vec::new();
	let mut variants_with_docs: Vec<(syn::Ident, Vec<Attribute>, syn::Fields)> = Vec::new();

	for variant in &enum_data.variants {
		let variant_name: &syn::Ident = &variant.ident;
		let variant_fields: &syn::Fields = &variant.fields;
		let mut variant_attrs: Vec<Attribute> = variant.attrs.clone();
		let mut generated_doc: Option<String> = None;

		let is_reserved: bool = has_attribute(&variant_attrs, "reserved");
		if is_reserved {
			reserved_variants.push(variant_name);
		}

		if let Some(keyword_text) = get_keyword_attr(&variant_attrs)? {
			keywords.push((keyword_text.clone(), variant_name.clone()));
			generated_doc = Some(format!(
				"Keyword: `{}`{}",
				keyword_text,
				if is_reserved { " (reserved)" } else { "" }
			));
		}

		if let Some(op_info) = get_operator_attr(&variant_attrs)? {
			operators.push((variant_name.clone(), op_info.clone()));
			generated_doc = Some(format!("Operator: `{}`", op_info.op_string));
		}

		if let Some(token_text) = get_simple_token_attr(&variant_attrs)? {
			simple_tokens.push((token_text.clone(), variant_name.clone()));
			generated_doc = Some(format!("Token: `{}`", token_text));
		}

		if let Some(doc) = generated_doc
			&& !variant_attrs.iter().any(|attr| return attr.path().is_ident("doc"))
		{
			let doc_attr: Attribute = syn::parse_quote! { #[doc = #doc] };
			variant_attrs.insert(0, doc_attr);
		}

		variant_attrs.retain(|attr| {
			return !attr.path().is_ident("keyword")
				&& !attr.path().is_ident("operator")
				&& !attr.path().is_ident("simple_token")
				&& !attr.path().is_ident("reserved");
		});

		variants_with_docs.push((variant_name.clone(), variant_attrs, variant_fields.clone()));
	}

	let variants_output = variants_with_docs.iter().map(|(name, attrs, fields)| {
		return quote! {
			#(#attrs)*
			#name #fields
		};
	});

	let keyword_match_arms = keywords.iter().map(|(text, variant)| {
		return quote! {
			#text => return Some(#enum_name::#variant),
		};
	});

	let simple_token_match_arms = simple_tokens.iter().map(|(text, variant)| {
		let ch: char = text.chars().next().expect("");
		return quote! {
			#ch => {
				self.advance();
				#enum_name::#variant
			}
		};
	});

	let operator_match_arms: Vec<proc_macro2::TokenStream> = generate_operator_char_matches(&operators);

	let operator_methods: Vec<proc_macro2::TokenStream> = generate_operator_methods(enum_name, &operators);

	let reserved_check_arms = reserved_variants.iter().map(|variant| {
		return quote! {
			#enum_name::#variant => Err(()),
		};
	});

	let generics: &syn::Generics = &input.generics;
	let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

	return Ok(quote! {
		#(#enum_attrs)*
		#enum_vis enum #enum_name #generics {
			#(#variants_output,)*
		}

		impl<'source, 'config> Lexer<'source, 'config> {
			/// Generated method to match keywords from identifier text
			#[inline]
			pub(crate) fn match_keyword(ident: &str) -> Option<#enum_name> {
				match ident {
					#(#keyword_match_arms)*
					_ => None,
				}
			}

			/// Generated character matching logic for the lexer
			/// This handles all operators and simple tokens based on their attributes
			#[inline]
			pub(crate) fn lex_char(&mut self, ch: char) -> #enum_name {
				match ch {
					#(#operator_match_arms)*
					#(#simple_token_match_arms)*
					_ => {
						self.advance();
						#enum_name::Invalid
					}
				}
			}

			/// Generated operator lexing methods
			#(#operator_methods)*
		}

		impl #impl_generics #enum_name #ty_generics #where_clause {
			/// Check if this token kind is reserved
			pub fn check_reserved(&self) -> Result<(), ()> {
				match self {
					#(#reserved_check_arms)*
					_ => Ok(()),
				}
			}
		}
	});
}

fn has_attribute(attrs: &[Attribute], name: &str) -> bool
{
	return attrs.iter().any(|attr| return attr.path().is_ident(name));
}

fn get_keyword_attr(attrs: &[Attribute]) -> Result<Option<String>>
{
	for attr in attrs {
		if attr.path().is_ident("keyword") {
			let value: syn::LitStr = attr.parse_args()?;
			return Ok(Some(value.value()));
		}
	}
	return Ok(None);
}

fn get_simple_token_attr(attrs: &[Attribute]) -> Result<Option<String>>
{
	for attr in attrs {
		if attr.path().is_ident("simple_token") {
			let value: syn::LitStr = attr.parse_args()?;
			return Ok(Some(value.value()));
		}
	}
	return Ok(None);
}

#[derive(Debug, Clone)]
struct OperatorInfo
{
	op_string: String, // The operator string (e.g., "+", "+=", "<<", "<<=")
	first_char: char,
}

fn get_operator_attr(attrs: &[Attribute]) -> Result<Option<OperatorInfo>>
{
	for attr in attrs {
		if attr.path().is_ident("operator") {
			return Ok(Some(parse_operator_attr(attr)?));
		}
	}
	return Ok(None);
}

fn parse_operator_attr(attr: &Attribute) -> Result<OperatorInfo>
{
	let op_str: syn::LitStr = attr.parse_args()?;
	let op_string: String = op_str.value();

	let first_char: char = op_string
		.chars()
		.next()
		.ok_or_else(|| return Error::new(attr.span(), "operator string cannot be empty"))?;

	return Ok(OperatorInfo { op_string, first_char });
}

fn generate_operator_char_matches(operators: &[(syn::Ident, OperatorInfo)]) -> Vec<proc_macro2::TokenStream>
{
	let mut families: std::collections::HashMap<char, Vec<(syn::Ident, String)>> = std::collections::HashMap::new();

	for (variant, info) in operators {
		families
			.entry(info.first_char)
			.or_default()
			.push((variant.clone(), info.op_string.clone()));
	}

	return families
		.keys()
		.map(|&first_char| {
			let method_name = get_family_method_name(first_char);
			return quote! {
				#first_char => self.#method_name(),
			};
		})
		.collect();
}

fn get_family_method_name(first_char: char) -> syn::Ident
{
	return match first_char {
		// hardcoded for cleaner names
		'+' => syn::Ident::new("lex_plus_family", proc_macro2::Span::call_site()),
		'-' => syn::Ident::new("lex_minus_family", proc_macro2::Span::call_site()),
		'*' => syn::Ident::new("lex_star_family", proc_macro2::Span::call_site()),
		'/' => syn::Ident::new("lex_slash_family", proc_macro2::Span::call_site()),
		'%' => syn::Ident::new("lex_mod_family", proc_macro2::Span::call_site()),
		'<' => syn::Ident::new("lex_less_family", proc_macro2::Span::call_site()),
		'>' => syn::Ident::new("lex_greater_family", proc_macro2::Span::call_site()),
		'=' => syn::Ident::new("lex_equals_family", proc_macro2::Span::call_site()),
		'&' => syn::Ident::new("lex_ampersand_family", proc_macro2::Span::call_site()),
		'|' => syn::Ident::new("lex_pipe_family", proc_macro2::Span::call_site()),
		'^' => syn::Ident::new("lex_caret_family", proc_macro2::Span::call_site()),
		'~' => syn::Ident::new("lex_tilde_family", proc_macro2::Span::call_site()),
		'!' => syn::Ident::new("lex_bang_family", proc_macro2::Span::call_site()),
		'.' => syn::Ident::new("lex_dot_family", proc_macro2::Span::call_site()),
		':' => syn::Ident::new("lex_colon_family", proc_macro2::Span::call_site()),
		_ => syn::Ident::new(
			&format!("lex_char_{}_family", first_char as u32),
			proc_macro2::Span::call_site(),
		),
	};
}

fn generate_operator_methods(
	enum_name: &syn::Ident,
	operators: &[(syn::Ident, OperatorInfo)],
) -> Vec<proc_macro2::TokenStream>
{
	let mut families: std::collections::HashMap<char, Vec<(syn::Ident, String)>> = std::collections::HashMap::new();

	for (variant, info) in operators {
		families
			.entry(info.first_char)
			.or_default()
			.push((variant.clone(), info.op_string.clone()));
	}

	return families
		.into_iter()
		.map(|(first_char, mut ops)| {
			ops.sort_by_key(|(_, s)| return std::cmp::Reverse(s.len()));

			let method_name = get_family_method_name(first_char);

			let checks = generate_operator_checks(&ops, enum_name);

			return quote! {
				#[inline]
				fn #method_name(&mut self) -> #enum_name {
					self.advance(); // consume first character
					#checks
				}
			};
		})
		.collect();
}

fn generate_operator_checks(ops: &[(syn::Ident, String)], enum_name: &syn::Ident) -> proc_macro2::TokenStream
{
	if ops.is_empty() {
		return quote! { #enum_name::Invalid };
	}

	let mut threechar = Vec::new();
	let mut twochar = Vec::new();
	let mut onechar = None;

	for (variant, op_str) in ops {
		let rest: Vec<char> = op_str.chars().skip(1).collect();
		match rest.len() {
			0 => onechar = Some(variant),
			1 => twochar.push((variant, rest[0])),
			2 => threechar.push((variant, rest[0], rest[1])),
			_ => {
				unimplemented!("for now operators longer than 3 characters are not supported")
			}
		}
	}

	let mut result: proc_macro2::TokenStream = onechar.map_or_else(
		|| quote! { #enum_name::Invalid },
		|variant| quote! { #enum_name::#variant },
	);

	for (variant, ch) in twochar.iter().rev() {
		let fallback = result;
		result = quote! {
			match self.current_char {
				Some(#ch) => {
					self.advance();
					#enum_name::#variant
				}
				_ => #fallback
			}
		};
	}

	for (variant, ch1, ch2) in threechar.iter().rev() {
		let fallback: proc_macro2::TokenStream = result;
		result = quote! {
			match self.current_char {
				Some(#ch1) => {
					let next_pos = self.position + if let Some(c) = self.current_char { c.len_utf8() } else { 0 };
					let third_char = self.source.get(next_pos..).and_then(|s| s.chars().next());
					if third_char == Some(#ch2) {
						self.advance(); // consume second char
						self.advance(); // consume third char
						#enum_name::#variant
					} else {
						#fallback
					}
				}
				_ => #fallback
			}
		};
	}

	return result;
}
