use proc_macro::{TokenStream, TokenTree};
use proc_macro2::TokenStream as TokenStream2;
use quote::quote;

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

use syn::parse::ParseStream;
use syn::{Expr, Result, Token, parse::Parse};

struct CompilerBugInput
{
	span: Expr,
	_comma: Token![,],
	message: Expr,
}

impl Parse for CompilerBugInput
{
	fn parse(input: ParseStream) -> Result<Self>
	{
		return Ok(Self {
			span: input.parse()?,
			_comma: input.parse()?,
			message: input.parse()?,
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
	let message = input.message;

	return quote! {
		{
			crate::diagnostics::DiagnosticBuilder::bug("internal compiler bug")
				.code(crate::diagnostics::ErrorCode::CompilerBug)
				.primary(#span, Some(#message.to_string()))
				.note("this is a bug in the compiler, not your code")
				.help("please report this issue with a minimal reproduction")
		}
	};
}
