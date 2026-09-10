use proc_macro::TokenStream;
use quote::quote;
use syn::{Attribute, Data, DeriveInput, parse_macro_input, spanned::Spanned};

#[derive(Debug, Clone)]
struct OperatorInfo
{
	op_string: String,
	first_char: char,
	span: proc_macro2::Span,
}

#[derive(Debug, Clone)]
struct AutomaticTestConfig
{
	mod_name: syn::Ident,
	template: proc_macro2::TokenStream,
}

#[derive(Debug, Clone, Default)]
struct VariantTestOverride
{
	skip: bool,
	name: Option<syn::Ident>,
}

enum AutomaticTestItem
{
	Mod(syn::Ident),
	Function(proc_macro2::TokenStream),
}

impl syn::parse::Parse for AutomaticTestItem
{
	fn parse(input: syn::parse::ParseStream) -> syn::Result<Self>
	{
		// `mod` is a reserved keyword, so a plain `Ident` parse would reject it;
		// `parse_any` (from `syn::ext::IdentExt`) accepts keywords as idents too.
		use syn::ext::IdentExt as _;
		let ident: syn::Ident = syn::Ident::parse_any(input)?;
		let content;
		syn::parenthesized!(content in input);
		if ident == "mod" {
			let name: syn::Ident = content.parse()?;
			return Ok(AutomaticTestItem::Mod(name));
		} else if ident == "function" {
			let tokens: proc_macro2::TokenStream = content.parse()?;
			return Ok(AutomaticTestItem::Function(tokens));
		}
		return Err(syn::Error::new(
			ident.span(),
			"expected `mod(module_name)` or `function( ...fn tokens... )`",
		));
	}
}

enum VariantTestItem
{
	Skip,
	Name(syn::Ident),
}

impl syn::parse::Parse for VariantTestItem
{
	fn parse(input: syn::parse::ParseStream) -> syn::Result<Self>
	{
		let ident: syn::Ident = input.parse()?;
		if ident == "skip" {
			return Ok(VariantTestItem::Skip);
		} else if ident == "name" {
			let content;
			syn::parenthesized!(content in input);
			let name: syn::Ident = content.parse()?;
			return Ok(VariantTestItem::Name(name));
		}
		return Err(syn::Error::new(
			ident.span(),
			"expected `skip` or `name(custom_test_name)`",
		));
	}
}

#[derive(Debug, Clone)]
struct TestableToken
{
	variant_name: syn::Ident,
	token_text: String,
	override_: VariantTestOverride,
}

pub fn generate_lexer(attr: TokenStream, item: TokenStream) -> TokenStream
{
	let input: DeriveInput = parse_macro_input!(item as DeriveInput);
	let nattr: proc_macro2::TokenStream = attr.into();

	return match generate_lexer_impl(nattr, &input) {
		Ok(tokens) => tokens.into(),
		Err(err) => err.to_compile_error().into(),
	};
}

fn generate_lexer_impl(attr: proc_macro2::TokenStream, input: &DeriveInput) -> syn::Result<proc_macro2::TokenStream>
{
	if attr.is_empty() {
		return Err(syn::Error::new(
			proc_macro2::Span::call_site(),
			"generate_lexer requires the lexer struct name as an argument",
		));
	}
	let attr_span: proc_macro2::Span = attr.span();
	let lexer_type: syn::Type = syn::parse2(attr)?;

	let lexer_lifetimes: Vec<syn::Lifetime> = match &lexer_type {
		syn::Type::Path(type_path) => {
			let last_segment = type_path.path.segments.last().ok_or_else(|| {
				return syn::Error::new(attr_span, "expected a lexer struct name");
			})?;
			match &last_segment.arguments {
				syn::PathArguments::None => Vec::new(),
				syn::PathArguments::AngleBracketed(args) => args
					.args
					.iter()
					.filter_map(|arg| {
						return match arg {
							syn::GenericArgument::Lifetime(lt) => Some(lt.clone()),
							_ => None,
						};
					})
					.collect(),
				syn::PathArguments::Parenthesized(_) => {
					return Err(syn::Error::new(attr_span, "unexpected parenthesized args"));
				}
			}
		}
		_ => {
			return Err(syn::Error::new(attr_span, "expected a lexer struct name"));
		}
	};

	let enum_data: &syn::DataEnum = match &input.data {
		Data::Enum(data) => data,
		_ => {
			return Err(syn::Error::new(
				input.span(),
				"generate_lexer can only be used on enums",
			));
		}
	};

	let enum_name: &syn::Ident = &input.ident;
	let enum_vis: &syn::Visibility = &input.vis;

	// NEW: own the attrs so we can strip out #[automatic_test(...)] before re-emitting.
	let mut enum_attrs: Vec<Attribute> = input.attrs.clone();
	let automatic_test_config: Option<AutomaticTestConfig> = get_automatic_test_config(&enum_attrs)?;
	enum_attrs.retain(|attr| return !attr.path().is_ident("automatic_test"));

	let mut keywords: Vec<(String, syn::Ident)> = Vec::new();
	let mut seen_keywords: std::collections::HashMap<String, syn::Ident> = std::collections::HashMap::new();

	let mut operators: Vec<(syn::Ident, OperatorInfo)> = Vec::new();
	let mut simple_tokens: Vec<(String, syn::Ident)> = Vec::new();
	let mut reserved_variants: Vec<&syn::Ident> = Vec::new();
	let mut variants_with_docs: Vec<(syn::Ident, Vec<Attribute>, syn::Fields)> = Vec::new();
	let mut testable_tokens: Vec<TestableToken> = Vec::new();

	let mut claimed_first_chars: std::collections::HashMap<char, String> = std::collections::HashMap::new();

	for variant in &enum_data.variants {
		let variant_name: &syn::Ident = &variant.ident;
		let variant_fields: &syn::Fields = &variant.fields;
		let mut variant_attrs: Vec<Attribute> = variant.attrs.clone();
		let mut generated_doc: Option<String> = None;

		let is_reserved: bool = has_attribute(&variant_attrs, "reserved");
		if is_reserved {
			reserved_variants.push(variant_name);
		}

		let keyword_attr: Option<String> = get_keyword_attr(&variant_attrs)?;
		let operator_attr: Option<OperatorInfo> = get_operator_attr(&variant_attrs)?;
		let simple_token_attr: Option<String> = get_simple_token_attr(&variant_attrs)?;
		let test_override: VariantTestOverride = get_variant_test_override(&variant_attrs)?;

		#[allow(clippy::cast_lossless)]
		let role_count = keyword_attr.is_some() as u8 + operator_attr.is_some() as u8 + simple_token_attr.is_some() as u8;
		if role_count > 1 {
			return Err(syn::Error::new(
				variant.span(),
				format!("variant `{variant_name}` has more than one role attribute"),
			));
		}

		if let Some(keyword_text) = keyword_attr.clone() {
			if let Some(prev) = seen_keywords.insert(keyword_text.clone(), variant_name.clone()) {
				return Err(syn::Error::new(
					variant.span(),
					format!("keyword \"{keyword_text}\" already used by `{prev}`"),
				));
			}
			keywords.push((keyword_text.clone(), variant_name.clone()));
			generated_doc = Some(format!("Keyword: `{keyword_text}`"));
			testable_tokens.push(TestableToken {
				variant_name: variant_name.clone(),
				token_text: keyword_text,
				override_: test_override.clone(),
			});
		}

		if let Some(op_info) = operator_attr.clone() {
			operators.push((variant_name.clone(), op_info.clone()));
			generated_doc = Some(format!("Operator: `{}`", op_info.op_string));
			testable_tokens.push(TestableToken {
				variant_name: variant_name.clone(),
				token_text: op_info.op_string,
				override_: test_override.clone(),
			});
		}

		if let Some(token_text) = simple_token_attr.clone() {
			let ch: char = token_text.chars().next().expect("validated non-empty");
			claim_first_char(
				&mut claimed_first_chars,
				ch,
				format!("simple_token \"{token_text}\" (variant `{variant_name}`)"),
				variant.span(),
			)?;
			simple_tokens.push((token_text.clone(), variant_name.clone()));
			generated_doc = Some(format!("Token: `{token_text}`"));
			testable_tokens.push(TestableToken {
				variant_name: variant_name.clone(),
				token_text,
				override_: test_override.clone(),
			});
		}

		if !test_override.skip && test_override.name.is_some() {
			let has_role = keyword_attr.is_some() || operator_attr.is_some() || simple_token_attr.is_some();
			if !has_role {
				return Err(syn::Error::new(
					variant.span(),
					format!(
						"variant `{variant_name}` has #[automatic_test(...)] but no \
                         #[keyword]/#[operator]/#[simple_token] to generate a test from",
					),
				));
			}
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
				&& !attr.path().is_ident("reserved")
				&& !attr.path().is_ident("automatic_test");
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
		return quote! { #text => return Some(#enum_name::#variant), };
	});

	let simple_token_match_arms = simple_tokens.iter().map(|(text, variant)| {
		let ch: char = text.chars().next().expect("validated non-empty");
		return quote! {
			#ch => { self.advance(); #enum_name::#variant }
		};
	});

	let operator_match_arms: Vec<proc_macro2::TokenStream> = generate_operator_char_matches(&operators);
	let operator_methods: Vec<proc_macro2::TokenStream> =
		generate_operator_methods(enum_name, &input.generics, &operators)?;
	let operator_methods_section: proc_macro2::TokenStream = if operator_methods.is_empty() {
		quote! {}
	} else {
		quote! { #(#operator_methods)* }
	};

	let reserved_check_arms = reserved_variants.iter().map(|variant| {
		return quote! { #enum_name::#variant => Err(()), };
	});

	let generics: &syn::Generics = &input.generics;
	let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

	// NEW: build the generated test module, if requested.
	let test_module: proc_macro2::TokenStream = match automatic_test_config {
		Some(config) => generate_automatic_tests(&config, &testable_tokens)?,
		None => quote! {},
	};

	return Ok(quote! {
		#(#enum_attrs)*
		#enum_vis enum #enum_name #generics {
			#(#variants_output,)*
		}

		impl<#(#lexer_lifetimes),*> #lexer_type {
			#[inline]
			pub(crate) fn match_keyword(ident: &str) -> Option<#enum_name #generics> {
				match ident {
					#(#keyword_match_arms)*
					_ => None,
				}
			}

			#[inline]
			pub(crate) fn lex_char(&mut self, ch: char) -> #enum_name #generics {
				match ch {
					#(#operator_match_arms)*
					#(#simple_token_match_arms)*
					_ => { self.advance(); #enum_name::Invalid }
				}
			}

			#operator_methods_section
		}

		impl #impl_generics #enum_name #ty_generics #where_clause {
			pub fn check_reserved(&self) -> Result<(), ()> {
				match self {
					#(#reserved_check_arms)*
					_ => Ok(()),
				}
			}
		}

		#test_module
	});
}

// ===== NEW: automatic_test support =====

fn get_automatic_test_config(attrs: &[Attribute]) -> syn::Result<Option<AutomaticTestConfig>>
{
	for attr in attrs {
		if attr.path().is_ident("automatic_test") {
			let items = attr
				.parse_args_with(syn::punctuated::Punctuated::<AutomaticTestItem, syn::Token![,]>::parse_terminated)?;

			let mut mod_name: Option<syn::Ident> = None;
			let mut template: Option<proc_macro2::TokenStream> = None;
			for item in items {
				match item {
					AutomaticTestItem::Mod(name) => mod_name = Some(name),
					AutomaticTestItem::Function(tokens) => template = Some(tokens),
				}
			}

			let nmod_name = mod_name.ok_or_else(|| {
				return syn::Error::new(
					attr.span(),
					"#[automatic_test(...)] on the enum requires `mod(module_name)`",
				);
			})?;
			let ntemplate = template.ok_or_else(|| {
				return syn::Error::new(
					attr.span(),
					"#[automatic_test(...)] on the enum requires `function( ...fn tokens... )`",
				);
			})?;

			return Ok(Some(AutomaticTestConfig {
				mod_name: nmod_name,
				template: ntemplate,
			}));
		}
	}
	return Ok(None);
}

fn get_variant_test_override(attrs: &[Attribute]) -> syn::Result<VariantTestOverride>
{
	let mut result = VariantTestOverride::default();
	for attr in attrs {
		if attr.path().is_ident("automatic_test") {
			if matches!(attr.meta, syn::Meta::Path(_)) {
				// Bare `#[automatic_test]` on a variant is shorthand for `skip`.
				result.skip = true;
				continue;
			}
			let items =
				attr.parse_args_with(syn::punctuated::Punctuated::<VariantTestItem, syn::Token![,]>::parse_terminated)?;
			for item in items {
				match item {
					VariantTestItem::Skip => result.skip = true,
					VariantTestItem::Name(name) => result.name = Some(name),
				}
			}
		}
	}
	return Ok(result);
}

fn to_snake_case(input: &str) -> String
{
	let mut result = String::new();
	for (i, ch) in input.chars().enumerate() {
		if ch.is_uppercase() {
			if i != 0 {
				result.push('_');
			}
			result.extend(ch.to_lowercase());
		} else {
			result.push(ch);
		}
	}
	return result;
}

/// Walks `input`, replacing every `# ident` pair (mirroring `quote!`'s `#var`
/// interpolation syntax) whose `ident` matches a key in `subs` with the
/// corresponding token(s). Recurses into groups (`{ }`, `( )`, `[ ]`) so
/// placeholders can appear anywhere in the template, including nested in the
/// function body.
fn substitute_tokens(
	input: proc_macro2::TokenStream,
	subs: &std::collections::HashMap<String, proc_macro2::TokenStream>,
) -> proc_macro2::TokenStream
{
	let mut output = proc_macro2::TokenStream::new();
	let mut iter = input.into_iter().peekable();

	while let Some(tt) = iter.next() {
		match &tt {
			proc_macro2::TokenTree::Punct(p) if p.as_char() == '#' => {
				if let Some(proc_macro2::TokenTree::Ident(ident)) = iter.peek()
					&& let Some(replacement) = subs.get(&ident.to_string())
				{
					output.extend(replacement.clone());
					iter.next(); // consume the placeholder ident
					continue;
				}
				output.extend(std::iter::once(tt));
			}
			proc_macro2::TokenTree::Group(g) => {
				let new_inner = substitute_tokens(g.stream(), subs);
				let mut new_group = proc_macro2::Group::new(g.delimiter(), new_inner);
				new_group.set_span(g.span());
				output.extend(std::iter::once(proc_macro2::TokenTree::Group(new_group)));
			}
			_ => {
				output.extend(std::iter::once(tt));
			}
		}
	}

	return output;
}

fn generate_automatic_tests(
	config: &AutomaticTestConfig,
	testable_tokens: &[TestableToken],
) -> syn::Result<proc_macro2::TokenStream>
{
	let mod_name = &config.mod_name;

	let mut seen_names: std::collections::HashSet<String> = std::collections::HashSet::new();
	let mut fns: Vec<proc_macro2::TokenStream> = Vec::new();

	for token in testable_tokens {
		if token.override_.skip {
			continue;
		}

		let test_name_ident: syn::Ident = token.override_.name.clone().unwrap_or_else(|| {
			let name = format!("test_lex_{}", to_snake_case(&token.variant_name.to_string()));
			return syn::Ident::new(&name, token.variant_name.span());
		});

		if !seen_names.insert(test_name_ident.to_string()) {
			return Err(syn::Error::new(
				token.variant_name.span(),
				format!(
					"automatic_test name `{test_name_ident}` collides with another generated \
                     test; use #[automatic_test(name(...))] to disambiguate",
				),
			));
		}

		// Build the #placeholder -> tokens substitution map, quote!-style.
		let token_str_literal = proc_macro2::Literal::string(&token.token_text);
		let variant_ident = token.variant_name.clone();

		let subs: std::collections::HashMap<String, proc_macro2::TokenStream> = std::collections::HashMap::from([
			("test_name".to_string(), quote! { #test_name_ident }),
			("token_str".to_string(), quote! { #token_str_literal }),
			("variant".to_string(), quote! { #variant_ident }),
		]);

		let rendered: proc_macro2::TokenStream = substitute_tokens(config.template.clone(), &subs);

		let item_fn: syn::ItemFn = syn::parse2(rendered.clone()).map_err(|err| {
			return syn::Error::new(
				token.variant_name.span(),
				format!(
					"automatic_test template did not produce a valid function for variant \
                     `{}`: {err}\n---rendered---\n{rendered}\n---",
					token.variant_name,
				),
			);
		})?;

		fns.push(quote! {
			#[test]
			#item_fn
		});
	}

	return Ok(quote! {
		#[cfg(test)]
		mod #mod_name {
			use super::*;

			#(#fns)*
		}
	});
}

// ===== existing helpers (unchanged) =====

fn claim_first_char(
	claimed: &mut std::collections::HashMap<char, String>,
	first_char: char,
	owner_description: String,
	span: proc_macro2::Span,
) -> syn::Result<()>
{
	if let Some(prev) = claimed.get(&first_char) {
		return Err(syn::Error::new(
			span,
			format!("'{first_char}' is already claimed by {prev}; cannot also be used by {owner_description}"),
		));
	}
	claimed.insert(first_char, owner_description);
	return Ok(());
}

fn has_attribute(attrs: &[Attribute], name: &str) -> bool
{
	return attrs.iter().any(|attr| return attr.path().is_ident(name));
}

fn get_keyword_attr(attrs: &[Attribute]) -> syn::Result<Option<String>>
{
	for attr in attrs {
		if attr.path().is_ident("keyword") {
			let value: syn::LitStr = attr.parse_args()?;
			return Ok(Some(value.value()));
		}
	}
	return Ok(None);
}

fn get_simple_token_attr(attrs: &[Attribute]) -> syn::Result<Option<String>>
{
	for attr in attrs {
		if attr.path().is_ident("simple_token") {
			let value: syn::LitStr = attr.parse_args()?;
			let text = value.value();
			if text.chars().count() != 1 {
				return Err(syn::Error::new(
					attr.span(),
					"simple_token must be exactly one character",
				));
			}
			return Ok(Some(text));
		}
	}
	return Ok(None);
}

fn get_operator_attr(attrs: &[Attribute]) -> syn::Result<Option<OperatorInfo>>
{
	for attr in attrs {
		if attr.path().is_ident("operator") {
			return Ok(Some(parse_operator_attr(attr)?));
		}
	}
	return Ok(None);
}

fn parse_operator_attr(attr: &Attribute) -> syn::Result<OperatorInfo>
{
	let op_str: syn::LitStr = attr.parse_args()?;
	let op_string: String = op_str.value();
	let first_char: char = op_string
		.chars()
		.next()
		.ok_or_else(|| return syn::Error::new(attr.span(), "operator string cannot be empty"))?;
	return Ok(OperatorInfo {
		op_string,
		first_char,
		span: attr.span(),
	});
}

fn generate_operator_char_matches(operators: &[(syn::Ident, OperatorInfo)]) -> Vec<proc_macro2::TokenStream>
{
	let families: std::collections::BTreeMap<char, Vec<(syn::Ident, String)>> = group_by_first_char(operators);
	return families
		.keys()
		.map(|&first_char| {
			let method_name = get_family_method_name(first_char);
			return quote! { #first_char => self.#method_name(), };
		})
		.collect();
}

fn get_family_method_name(first_char: char) -> syn::Ident
{
	return match first_char {
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

fn group_by_first_char(
	operators: &[(syn::Ident, OperatorInfo)],
) -> std::collections::BTreeMap<char, Vec<(syn::Ident, String)>>
{
	let mut families: std::collections::BTreeMap<char, Vec<(syn::Ident, String)>> = std::collections::BTreeMap::new();
	for (variant, info) in operators {
		families
			.entry(info.first_char)
			.or_default()
			.push((variant.clone(), info.op_string.clone()));
	}
	return families;
}

fn generate_operator_methods(
	enum_name: &syn::Ident,
	generics: &syn::Generics,
	operators: &[(syn::Ident, OperatorInfo)],
) -> syn::Result<Vec<proc_macro2::TokenStream>>
{
	let families: std::collections::BTreeMap<char, Vec<(syn::Ident, String)>> = group_by_first_char(operators);
	let spans: std::collections::HashMap<(char, String), proc_macro2::Span> = operators
		.iter()
		.map(|(_, info)| return ((info.first_char, info.op_string.clone()), info.span))
		.collect();

	return families
		.into_iter()
		.map(|(first_char, mut ops)| {
			ops.sort_by_key(|(_, s)| return std::cmp::Reverse(s.len()));
			let method_name: syn::Ident = get_family_method_name(first_char);
			let checks: proc_macro2::TokenStream = generate_operator_checks(&ops, enum_name, &spans)?;
			return Ok(quote! {
				#[inline]
				fn #method_name(&mut self) -> #enum_name #generics {
					self.advance();
					#checks
				}
			});
		})
		.collect();
}

fn generate_operator_checks(
	ops: &[(syn::Ident, String)],
	enum_name: &syn::Ident,
	spans: &std::collections::HashMap<(char, String), proc_macro2::Span>,
) -> syn::Result<proc_macro2::TokenStream>
{
	if ops.is_empty() {
		return Ok(quote! { #enum_name::Invalid });
	}

	let mut threechar: Vec<(&syn::Ident, char, char)> = Vec::new();
	let mut twochar: Vec<(&syn::Ident, char)> = Vec::new();
	let mut onechar: Option<&syn::Ident> = None;

	for (variant, op_str) in ops {
		let rest: Vec<char> = op_str.chars().skip(1).collect();
		match rest.len() {
			0 => onechar = Some(variant),
			1 => twochar.push((variant, rest[0])),
			2 => threechar.push((variant, rest[0], rest[1])),
			_ => {
				let first_char: char = op_str.chars().next().unwrap_or_default();
				let span: proc_macro2::Span = spans
					.get(&(first_char, op_str.clone()))
					.copied()
					.unwrap_or_else(proc_macro2::Span::call_site);
				return Err(syn::Error::new(
					span,
					format!("operator \"{op_str}\" is longer than 3 characters"),
				));
			}
		}
	}

	let mut result: proc_macro2::TokenStream = onechar.map_or_else(
		|| quote! { #enum_name::Invalid },
		|variant| quote! { #enum_name::#variant },
	);

	for (variant, ch) in twochar.iter().rev() {
		let fallback: proc_macro2::TokenStream = result;
		result = quote! {
			match self.current_char {
				Some(#ch) => { self.advance(); #enum_name::#variant }
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
						self.advance();
						self.advance();
						#enum_name::#variant
					} else {
						#fallback
					}
				}
				_ => #fallback
			}
		};
	}

	return Ok(result);
}
