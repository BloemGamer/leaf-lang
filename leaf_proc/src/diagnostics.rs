use proc_macro::{TokenStream, TokenTree};
use proc_macro2::TokenStream as TokenStream2;
use quote::{format_ident, quote};
use syn::{Attribute, DeriveInput, Expr, Fields, Ident, LitStr, Token, parse::Parse, parse::ParseStream};

pub fn diagnostic_builder(item: TokenStream) -> TokenStream
{
	let mut tokens: proc_macro::token_stream::IntoIter = item.into_iter();
	let Some(TokenTree::Ident(error_name)) = tokens.next() else {
		todo!()
	};

	let Some(TokenTree::Group(error_def)) = tokens.next() else {
		todo!()
	};

	return TokenStream::from_iter([TokenTree::Ident(error_name), TokenTree::Group(error_def)])
		.into_iter()
		.chain(".build()".parse::<TokenStream>().expect(""))
		.collect();
}

struct CompilerBugInput
{
	span: Expr,
	_comma: Token![,],
	format_str: LitStr,
	format_args: Vec<Expr>,
}

impl Parse for CompilerBugInput
{
	fn parse(input: ParseStream) -> syn::Result<Self>
	{
		let span = input.parse()?;
		let _comma: Token![,] = input.parse()?;
		let format_str: LitStr = input.parse()?;
		let mut format_args = Vec::new();
		while input.peek(Token![,]) {
			input.parse::<Token![,]>()?;
			if input.is_empty() {
				break;
			}
			format_args.push(input.parse::<Expr>()?);
		}
		return Ok(Self {
			span,
			_comma,
			format_str,
			format_args,
		});
	}
}

pub fn compiler_bug(item: TokenStream2) -> TokenStream2
{
	let input = match syn::parse2::<CompilerBugInput>(item) {
		Ok(v) => v,
		Err(e) => return e.to_compile_error(),
	};

	let span = input.span;
	let format_str = input.format_str;
	let format_args = input.format_args;

	let input_span = proc_macro::Span::call_site();

	let source_file = input_span.file();
	let line = input_span.line();

	return quote! {
		{
			crate::diagnostics::DiagnosticBuilder::bug("internal compiler bug")
				.code(crate::diagnostics::ErrorCode::CompilerBug)
				.primary(#span, Some(format!(#format_str #(, #format_args)*)))
				.note("this is a bug in the compiler, not your code")
				.note(format!("{}:{}", #source_file, #line))
				.help("please report this issue with a minimal reproduction")
		}
	};
}

pub fn compiler_not_implemented(item: TokenStream2) -> TokenStream2
{
	let input = match syn::parse2::<CompilerBugInput>(item) {
		Ok(v) => v,
		Err(e) => return e.to_compile_error(),
	};

	let span = input.span;
	let format_str = input.format_str;
	let format_args = input.format_args;

	let input_span = proc_macro::Span::call_site();

	let source_file = input_span.file();
	let line = input_span.line();

	return quote! {
		{
			crate::diagnostics::DiagnosticBuilder::not_implemented("a not yet implemented feature")
				.code(crate::diagnostics::ErrorCode::CompilerNotImplemented)
				.primary(#span, Some(format!(#format_str #(, #format_args)*)))
				.note("this feature is not yet implemented")
				.note(format!("{}:{}", #source_file, #line))
		}
	};
}

struct ConstructorAttr
{
	fn_name: Ident,
	params: Vec<ConstructorParam>,
}

struct ConstructorParam
{
	name: Ident,
	ty: TokenStream2,
	/// true when the source wrote `impl Into<String>`
	is_into_string: bool,
}

impl Parse for ConstructorAttr
{
	fn parse(input: ParseStream) -> syn::Result<Self>
	{
		let fn_name: Ident = input.parse()?;
		let inner;
		syn::parenthesized!(inner in input);

		let mut params = Vec::new();
		while !inner.is_empty() {
			let name: Ident = inner.parse()?;
			inner.parse::<Token![:]>()?;

			let ty_tokens: TokenStream2 = {
				let mut ts = TokenStream2::new();
				while !inner.is_empty() {
					if inner.peek(Token![,]) {
						break;
					}
					let tt: proc_macro2::TokenTree = inner.parse()?;
					ts.extend(quote! { #tt });
				}
				ts
			};

			let ty_str = ty_tokens.to_string();
			let is_into_string = ty_str.contains("Into < String >")
				|| ty_str.contains("Into<String>")
				|| ty_str.replace(' ', "").contains("Into<String>");

			params.push(ConstructorParam {
				name,
				ty: ty_tokens,
				is_into_string,
			});

			if inner.peek(Token![,]) {
				inner.parse::<Token![,]>()?;
			}
		}

		return Ok(ConstructorAttr { fn_name, params });
	}
}

struct LabelAttr
{
	is_primary: bool,
	message: Option<LitStr>,
}

impl Parse for LabelAttr
{
	fn parse(input: ParseStream) -> syn::Result<Self>
	{
		let kind: Ident = input.parse()?;
		let is_primary = match kind.to_string().as_str() {
			"primary" => true,
			"secondary" => false,
			other => {
				return Err(syn::Error::new(
					kind.span(),
					format!("expected `primary` or `secondary`, found `{other}`"),
				));
			}
		};
		let message = if input.peek(Token![,]) {
			input.parse::<Token![,]>()?;
			Some(input.parse::<LitStr>()?)
		} else {
			None
		};
		return Ok(LabelAttr { is_primary, message });
	}
}

struct VariantMeta
{
	ident: Ident,
	fields: Fields,
	error_msg: LitStr,
	error_code: TokenStream2,
	constructor: Option<ConstructorAttr>,
	labels: Vec<LabelAttr>,
	notes: Vec<LitStr>,
	helps: Vec<LitStr>,
}

fn require_attr_litstr(attr: &Attribute, name: &str) -> syn::Result<LitStr>
{
	let inner: LitStr = attr.parse_args()?;
	let _: &str = name;
	return Ok(inner);
}

fn parse_error_code(attr: &Attribute) -> syn::Result<TokenStream2>
{
	let expr: Expr = attr.parse_args()?;
	return Ok(quote! { #expr });
}

fn attr_is(attr: &Attribute, name: &str) -> bool
{
	return attr.path().is_ident(name);
}

pub fn impl_parse_errors(input: &DeriveInput) -> syn::Result<TokenStream2>
{
	let kind_ident = &input.ident;

	let wrapper_name = {
		let s = kind_ident.to_string();
		if let Some(base) = s.strip_suffix("Kind") {
			format_ident!("{}", base)
		} else {
			return Err(syn::Error::new(
				kind_ident.span(),
				"ParseErrors: enum name must end in `Kind` (e.g. `ParseErrorKind`)",
			));
		}
	};

	let syn::Data::Enum(ref data) = input.data else {
		return Err(syn::Error::new_spanned(&input.ident, "ParseErrors only works on enums"));
	};

	let mut variants: Vec<VariantMeta> = Vec::new();

	for variant in &data.variants {
		let ident = variant.ident.clone();
		let fields = variant.fields.clone();

		let mut error_msg: Option<LitStr> = None;
		let mut error_code: Option<TokenStream2> = None;
		let mut constructor: Option<ConstructorAttr> = None;
		let mut labels: Vec<LabelAttr> = Vec::new();
		let mut notes: Vec<LitStr> = Vec::new();
		let mut helps: Vec<LitStr> = Vec::new();

		for attr in &variant.attrs {
			if attr_is(attr, "error_msg") {
				error_msg = Some(require_attr_litstr(attr, "error_msg")?);
			} else if attr_is(attr, "error_code") {
				error_code = Some(parse_error_code(attr)?);
			} else if attr_is(attr, "constructor") {
				constructor = Some(attr.parse_args::<ConstructorAttr>()?);
			} else if attr_is(attr, "label") {
				labels.push(attr.parse_args::<LabelAttr>()?);
			} else if attr_is(attr, "note") {
				notes.push(attr.parse_args::<LitStr>()?);
			} else if attr_is(attr, "help") {
				helps.push(attr.parse_args::<LitStr>()?);
			}
		}

		let nerror_msg = error_msg.ok_or_else(|| {
			return syn::Error::new(
				ident.span(),
				format!("variant `{ident}` is missing `#[error_msg(\"…\")]`"),
			);
		})?;
		let nerror_code = error_code.ok_or_else(|| {
			return syn::Error::new(ident.span(), format!("variant `{ident}` is missing `#[error_code(…)]`"));
		})?;

		variants.push(VariantMeta {
			ident,
			fields,
			error_msg: nerror_msg,
			error_code: nerror_code,
			constructor,
			labels,
			notes,
			helps,
		});
	}

	let mut compile_error_variant: Option<TokenStream2> = None;

	for attr in &input.attrs {
		if attr_is(attr, "compile_error_variant") {
			let expr: Expr = attr.parse_args()?;
			compile_error_variant = Some(quote! { #expr });
		}
	}

	let display_impl = gen_display(kind_ident, &variants);
	let constructor_impl = gen_constructors(&wrapper_name, kind_ident, &variants);
	let diagnostic_impl = gen_diagnostic(&wrapper_name, kind_ident, &variants);
	let from_impl = compile_error_variant.map_or_else(
		|| {
			return quote! {
				impl std::fmt::Display for #wrapper_name {
					fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
						return std::fmt::Display::fmt(&self.kind, f);
					}
				}
			};
		},
		|variant| {
			return quote! {
				impl std::fmt::Display for #wrapper_name {
					fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
						return std::fmt::Display::fmt(&self.kind, f);
					}
				}

				impl From<#wrapper_name> for CompileError {
					fn from(value: #wrapper_name) -> Self {
						return #variant(value);
					}
				}
			};
		},
	);

	return Ok(quote! {
		#display_impl
		#constructor_impl
		#diagnostic_impl
		#from_impl
	});
}

fn gen_display(kind_ident: &Ident, variants: &[VariantMeta]) -> TokenStream2
{
	let arms = variants.iter().map(|v| {
		let vname = &v.ident;
		let fmt_str = &v.error_msg;
		let field_pat = field_pattern(&v.fields);

		return quote! {
			Self::#vname #field_pat => {
				write!(f, #fmt_str)?;
			}
		};
	});

	return quote! {
		impl std::fmt::Display for #kind_ident {
			fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
				match self {
					#( #arms )*
				}
				return Ok(());
			}
		}
	};
}

fn gen_constructors(wrapper: &Ident, kind_ident: &Ident, variants: &[VariantMeta]) -> TokenStream2
{
	let fns: Vec<_> = variants
		.iter()
		.filter_map(|v| return v.constructor.as_ref().map(|c| return (v, c)))
		.map(|(v, ctor)| {
			let fn_name = &ctor.fn_name;
			let vname = &v.ident;

			let params = ctor.params.iter().map(|p| {
				let n = &p.name;
				let t = &p.ty;
				return quote! { #n: #t };
			});

			let kind_fields: Vec<_> = ctor.params.iter().filter(|p| return p.name != "span").collect();

			let field_inits = kind_fields.iter().map(|p| {
				let n = &p.name;
				return if p.is_into_string {
					quote! { #n: #n.into() }
				} else {
					quote! { #n }
				};
			});

			let kind_expr = match &v.fields {
				Fields::Unit => quote! { #kind_ident::#vname },
				Fields::Named(_) => quote! { #kind_ident::#vname { #( #field_inits ),* } },
				Fields::Unnamed(_) => {
					let vals = kind_fields.iter().map(|p| {
						let n = &p.name;
						return if p.is_into_string {
							quote! { #n.into() }
						} else {
							quote! { #n }
						};
					});
					quote! { #kind_ident::#vname( #( #vals ),* ) }
				}
			};

			return quote! {
				pub fn #fn_name( #( #params ),* ) -> Self {
					return Self { span, kind: #kind_expr };
				}
			};
		})
		.collect();

	if fns.is_empty() {
		return quote! {};
	}

	return quote! {
		#[allow(unused)]
		impl #wrapper {
			#( #fns )*
		}
	};
}

fn gen_diagnostic(wrapper: &Ident, kind_ident: &Ident, variants: &[VariantMeta]) -> TokenStream2
{
	let arms = variants.iter().map(|v| {
		let vname = &v.ident;
		let fmt_str = &v.error_msg;
		let code = &v.error_code;
		let field_pat = field_pattern(&v.fields);

		let label_calls: Vec<TokenStream2> = if v.labels.is_empty() {
			vec![quote! { diag = diag.primary(self.span, None); }]
		} else {
			v.labels
				.iter()
				.map(|l| {
					let method = if l.is_primary {
						format_ident!("primary")
					} else {
						format_ident!("secondary")
					};
					let msg = l
						.message
						.as_ref()
						.map_or_else(|| quote! { None }, |s| quote! { Some(format!(#s)) });
					return quote! { diag = diag.#method(self.span, #msg); };
				})
				.collect()
		};

		let note_calls = v.notes.iter().map(|n| return quote! { diag = diag.note(#n); });

		let help_calls = v.helps.iter().map(|h| return quote! { diag = diag.help(#h); });

		return quote! {
			#kind_ident::#vname #field_pat => {
				let mut diag = DiagnosticBuilder::error(format!(#fmt_str))
					.code(#code);
				#( #label_calls )*
				#( #note_calls )*
				#( #help_calls )*
				diag
			}
		};
	});

	return quote! {
		impl CompileDiagnostic for #wrapper {
			fn build(&self) -> DiagnosticBuilder {
				return match &self.kind {
					#( #arms ),*
				};
			}
		}
	};
}

fn field_pattern(fields: &Fields) -> TokenStream2
{
	return match fields {
		Fields::Unit => quote! {},
		Fields::Named(n) => {
			let names = n
				.named
				.iter()
				.map(|f| return f.ident.as_ref().expect("field always has a name"));
			quote! { { #( #names ),* } }
		}
		Fields::Unnamed(u) => {
			let names = (0..u.unnamed.len()).map(|i| format_ident!("f{}", i));
			quote! { ( #( #names ),* ) }
		}
	};
}
