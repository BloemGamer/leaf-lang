use crate::{
	diagnostics::{self, Diagnostic},
	lexer::{Lexer, Token, TokenKind},
	preparser::{
		Attribute, DeclMeta, Docs, FunctionDecl, InterfaceDecl, Modifier, ModifierKind, ModuleDecl, PreParsedUnit,
		PreParsedUnitResult, SpanNoEq, TypeDecl, VariableDecl,
	},
	util::span::{Span, Spanned},
};

use super::ExternLanguage;

struct PreParser<'s, L>
where
	L: Lexer<'s>,
{
	lexer: L,
	diagnostics: Vec<Diagnostic>,

	peeked: Option<Token<'s>>,
	last_span: Span,
}

impl<'s, L> PreParser<'s, L>
where
	L: Lexer<'s>,
{
	const fn new(lexer: L) -> Self
	{
		return Self {
			lexer,
			diagnostics: Vec::new(),
			peeked: None,
			last_span: Span::DUMMY,
		};
	}

	fn peek(&mut self) -> &Token<'s>
	{
		if self.peeked.is_none() {
			self.peeked = self.lexer.next();
		}
		return self.peeked.as_ref().expect("lexer should always yield at least Eof");
	}

	fn bump(&mut self) -> Token<'s>
	{
		let token: Token<'s> = self
			.peeked
			.take()
			.or_else(|| return self.lexer.next())
			.expect("lexer should always yield at least Eof");
		self.last_span = token.span;
		return token;
	}

	#[track_caller]
	fn unexpected_top_level_token(&self, token: &Token<'s>) -> Diagnostic
	{
		return Diagnostic::error("got unexpected token")
			.primary(token.span(), Some(format!("got `{:?}`", token.kind)));
	}

	#[track_caller]
	fn expect(&mut self, kind: &TokenKind) -> Option<Token<'s>>
	{
		if self.peek().kind == *kind {
			return Some(self.bump());
		}
		let tok: Token<'s> = self.peek().clone();
		let diagnostic: Diagnostic = self.expected_token(&kind, &tok);
		self.diagnostics.push(diagnostic);
		return None;
	}

	#[track_caller]
	fn expect_and_recover(&mut self, kind: &TokenKind) -> Option<Token<'s>>
	{
		if self.peek().kind == *kind {
			return Some(self.bump());
		}
		let bad: Token<'s> = self.bump();
		let diagnostic: Diagnostic = self.expected_token(&kind, &bad);
		self.diagnostics.push(diagnostic);
		return None;
	}

	#[track_caller]
	fn expect_one_of(&mut self, kinds: &[TokenKind]) -> Option<Token<'s>>
	{
		if kinds.iter().any(|kind| return *kind == self.peek().kind) {
			return Some(self.bump());
		}
		let tok: Token<'s> = self.peek().clone();
		let diagnostic: Diagnostic = self.expected_one_of_tokens(kinds, &tok);
		self.diagnostics.push(diagnostic);
		return None;
	}

	fn eat(&mut self, kind: &TokenKind) -> bool
	{
		if self.peek().kind == *kind {
			self.bump();
			return true;
		}
		return false;
	}

	#[track_caller]
	fn expected_token(&self, expected: &TokenKind, got: &Token<'s>) -> Diagnostic
	{
		return Diagnostic::error("unexpected token").primary(
			got.span(),
			Some(format!("expected `{:?}`, got `{:?}`", expected, got.kind)),
		);
	}

	#[track_caller]
	fn expected_one_of_tokens(&self, expected: &[TokenKind], got: &Token<'s>) -> Diagnostic
	{
		let expected_str: String = expected
			.iter()
			.map(|kind| return format!("`{:?}`", kind))
			.collect::<Vec<_>>()
			.join(", ");
		return Diagnostic::error("unexpected token").primary(
			got.span(),
			Some(format!("expected one of {}, got `{:?}`", expected_str, got.kind)),
		);
	}

	fn parse(&mut self) -> PreParsedUnit<'s>
	{
		let start_span: Span = self.peek().span;
		let docs: Docs<'s> = self.parse_enclosed_docs();
		let attributes: Vec<Attribute<'s>> = self.parse_attributes();

		let mut modules: Vec<ModuleDecl<'s>> = Vec::new();
		let mut types: Vec<TypeDecl<'s>> = Vec::new();
		let mut interfaces: Vec<InterfaceDecl<'s>> = Vec::new();
		let mut variables: Vec<VariableDecl<'s>> = Vec::new();
		let mut functions: Vec<FunctionDecl<'s>> = Vec::new();

		loop {
			let decl_start_span: Span = self.peek().span;

			let meta: DeclMeta<'s> = {
				let docs: Docs<'s> = self.parse_leading_docs();
				let attributes: Vec<Attribute<'s>> = self.parse_attributes();
				let modifiers: Vec<Modifier> = self.parse_modifier();
				let span: Span = if self.last_span == Span::DUMMY {
					decl_start_span
				} else {
					self.last_span
				};
				DeclMeta {
					span: decl_start_span.to(span).into(),
					docs,
					attributes,
					modifiers,
				}
			};

			match self.peek().kind {
				TokenKind::Eof => break,

				TokenKind::Module => modules.push(self.parse_module_decl(meta)),
				TokenKind::Interface => interfaces.push(self.parse_interface_decl(meta)),
				TokenKind::Const | TokenKind::Static => variables.push(self.parse_variable_decl(meta)),
				TokenKind::Fn => functions.push(self.parse_function_decl(meta)),
				TokenKind::Type => types.push(self.parse_type_alias_decl(meta)),
				TokenKind::Struct => types.push(self.parse_struct_decl(meta)),
				TokenKind::Enum => types.push(self.parse_enum_decl(meta)),
				TokenKind::Variant => types.push(self.parse_variant_decl(meta)),
				TokenKind::Union => types.push(self.parse_union_decl(meta)),

				_ => {
					let bad: Token<'s> = self.bump();
					let diagnostic: Diagnostic = self.unexpected_top_level_token(&bad);
					self.diagnostics.push(diagnostic);
				}
			}
		}

		let span: SpanNoEq = start_span.to(self.last_span).into();

		return PreParsedUnit {
			span,
			docs,
			attributes,
			modules,
			types,
			interfaces,
			variables,
			functions,
		};
	}

	fn parse_leading_docs(&mut self) -> Docs<'s>
	{
		let mut span: Span = Span::DUMMY;
		let mut lines: Vec<&str> = Vec::new();
		if let tok @ Token {
			kind: TokenKind::DocsComment(str),
			..
		} = self.peek()
		{
			if span == Span::DUMMY {
				span = tok.span();
			} else {
				span = span.to(tok.span());
			}
			lines.push(str);
			self.bump();
		}
		return Docs {
			docs: lines,
			span: span.into(),
		};
	}

	fn parse_enclosed_docs(&mut self) -> Docs<'s>
	{
		let mut span: Span = Span::DUMMY;
		let mut lines: Vec<&str> = Vec::new();
		if let tok @ Token {
			kind: TokenKind::EnclosedDocsComment(str),
			..
		} = self.peek()
		{
			if span == Span::DUMMY {
				span = tok.span();
			} else {
				span = span.to(tok.span());
			}
			lines.push(str);
			self.bump();
		}
		return Docs {
			docs: lines,
			span: span.into(),
		};
	}

	fn parse_attributes(&mut self) -> Vec<Attribute<'s>>
	{
		todo!()
	}

	fn parse_modifier(&mut self) -> Vec<Modifier>
	{
		let mut modifiers: Vec<Modifier> = Vec::new();
		if matches!(self.peek().kind, TokenKind::Pub | TokenKind::Export) {
			match self.bump() {
				Token {
					kind: TokenKind::Export,
					span,
				} => {
					modifiers.push(Modifier {
						span: span.into(),
						kind: ModifierKind::Export,
					});
				}
				Token {
					kind: TokenKind::Pub,
					span,
				} => {
					modifiers.push(Modifier {
						span: span.into(),
						kind: ModifierKind::Pub,
					});
				}
				_ => unreachable!(),
			}
		}

		if matches!(self.peek().kind, TokenKind::Export) {
			let span_start: Span = self.bump().span(); // export
			self.expect(&TokenKind::LeftParen); // (
			let lang: ExternLanguage = match self.bump() {
				Token {
					kind: TokenKind::Identifier("C"),
					..
				} => ExternLanguage::C,
				tok => {
					self.diagnostics.push(
						Diagnostic::error("expected a valid language")
							.primary(tok.span(), Some(format!("got {:?}", tok.kind))),
					);
					ExternLanguage::C
				}
			};
			self.expect(&TokenKind::RightParen); // )
			modifiers.push(Modifier {
				span: span_start.to(self.last_span).into(),
				kind: ModifierKind::Extern(lang),
			});
		}

		if matches!(self.peek().kind, TokenKind::Unsafe) {
			let span: Span = self.bump().span();
			modifiers.push(Modifier {
				span: span.into(),
				kind: ModifierKind::Unsafe,
			});
		}

		if matches!(self.peek().kind, TokenKind::Const) {
			let span: Span = self.bump().span();
			modifiers.push(Modifier {
				span: span.into(),
				kind: ModifierKind::Const,
			});
		}

		return modifiers;
	}

	fn parse_module_decl(&mut self, meta: DeclMeta<'s>) -> ModuleDecl<'s>
	{
		todo!()
	}

	fn parse_interface_decl(&mut self, meta: DeclMeta<'s>) -> InterfaceDecl<'s>
	{
		todo!()
	}

	fn parse_variable_decl(&mut self, meta: DeclMeta<'s>) -> VariableDecl<'s>
	{
		todo!()
	}

	fn parse_type_alias_decl(&mut self, meta: DeclMeta<'s>) -> TypeDecl<'s>
	{
		todo!()
	}

	fn parse_function_decl(&mut self, meta: DeclMeta<'s>) -> FunctionDecl<'s>
	{
		todo!()
	}

	fn parse_struct_decl(&mut self, meta: DeclMeta<'s>) -> TypeDecl<'s>
	{
		todo!()
	}

	fn parse_enum_decl(&mut self, meta: DeclMeta<'s>) -> TypeDecl<'s>
	{
		todo!()
	}

	fn parse_variant_decl(&mut self, meta: DeclMeta<'s>) -> TypeDecl<'s>
	{
		todo!()
	}

	fn parse_union_decl(&mut self, meta: DeclMeta<'s>) -> TypeDecl<'s>
	{
		todo!()
	}
}

pub fn parse<'s, L>(lexer: L) -> PreParsedUnitResult<'s>
where
	L: Lexer<'s>,
{
	let mut parser: PreParser<'s, L> = PreParser::new(lexer);
	let pre_parsed_unit: PreParsedUnit<'s> = parser.parse();
	let diagnostics: Vec<Diagnostic> = parser.diagnostics;
	return PreParsedUnitResult {
		pre_parsed_unit,
		diagnostics,
	};
}
