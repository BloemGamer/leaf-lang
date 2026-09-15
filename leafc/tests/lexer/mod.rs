#![allow(clippy::needless_raw_string_hashes)]

use std::assert_matches;

use crate::{
	diagnostics::DiagnosticLevel,
	lexer::{BasicLexer, IntBase, IntSign, IntSize, IntType, Token, TokenKind},
	source_map::SourceIndex,
};

#[test]
fn string_normal()
{
	let mut lexer: BasicLexer<'_> = BasicLexer::new(r#" "test str" "#, SourceIndex::DUMMY);
	assert_matches!(lexer.next(), Some(Token { kind: TokenKind::StringLiteral { string, flags: _ }, span: _ }) if string == "test str");
	assert_matches!(
		lexer.next(),
		Some(Token {
			kind: TokenKind::Eof,
			span: _
		})
	);
}

//#[test]
//fn string_escape_n()
//{
//	let mut lexer: BasicLexer<'_> = BasicLexer::new(r#" "test\nstr" "#, SourceIndex::DUMMY);
//	assert_matches!(lexer.next(), Some(Token { kind: TokenKind::StringLiteral { string, flags: _ }, span: _ }) if string == "test\nstr");
//	assert_matches!(
//		lexer.next(),
//		Some(Token {
//			kind: TokenKind::Eof,
//			span: _
//		})
//	);
//}
//
//#[test]
//fn string_escape_t()
//{
//	let mut lexer: BasicLexer<'_> = BasicLexer::new(r#" "test\tstr" "#, SourceIndex::DUMMY);
//	assert_matches!(lexer.next(), Some(Token { kind: TokenKind::StringLiteral { string, flags: _ }, span: _ }) if string == "test\tstr");
//	assert_matches!(
//		lexer.next(),
//		Some(Token {
//			kind: TokenKind::Eof,
//			span: _
//		})
//	);
//}
//
//#[test]
//fn string_escape_r()
//{
//	let mut lexer: BasicLexer<'_> = BasicLexer::new(r#" "test\rstr" "#, SourceIndex::DUMMY);
//	assert_matches!(lexer.next(), Some(Token { kind: TokenKind::StringLiteral { string, flags: _ }, span: _ }) if string == "test\rstr");
//	assert_matches!(
//		lexer.next(),
//		Some(Token {
//			kind: TokenKind::Eof,
//			span: _
//		})
//	);
//}
//
//#[test]
//fn string_escape_0()
//{
//	let mut lexer: BasicLexer<'_> = BasicLexer::new(r#" "test\0str" "#, SourceIndex::DUMMY);
//	assert_matches!(lexer.next(), Some(Token { kind: TokenKind::StringLiteral { string, flags: _ }, span: _ }) if string == "test\0str");
//	assert_matches!(
//		lexer.next(),
//		Some(Token {
//			kind: TokenKind::Eof,
//			span: _
//		})
//	);
//}
//
//#[test]
//fn string_escape_backslash()
//{
//	let mut lexer: BasicLexer<'_> = BasicLexer::new(r#" "test\\str" "#, SourceIndex::DUMMY);
//	assert_matches!(lexer.next(), Some(Token { kind: TokenKind::StringLiteral { string, flags: _ }, span: _ }) if string == "test\\str");
//	assert_matches!(
//		lexer.next(),
//		Some(Token {
//			kind: TokenKind::Eof,
//			span: _
//		})
//	);
//}
//
//#[test]
//fn string_escape_single_quote()
//{
//	let mut lexer: BasicLexer<'_> = BasicLexer::new(r#" "test\'str" "#, SourceIndex::DUMMY);
//	assert_matches!(lexer.next(), Some(Token { kind: TokenKind::StringLiteral { string, flags: _ }, span: _ }) if string == "test\'str");
//	assert_matches!(
//		lexer.next(),
//		Some(Token {
//			kind: TokenKind::Eof,
//			span: _
//		})
//	);
//}
//
//#[test]
//fn string_escape_double_quote()
//{
//	let mut lexer: BasicLexer<'_> = BasicLexer::new(r#" "test\"str" "#, SourceIndex::DUMMY);
//	assert_matches!(lexer.next(), Some(Token { kind: TokenKind::StringLiteral { string, flags: _ }, span: _ }) if string == "test\"str");
//	assert_matches!(
//		lexer.next(),
//		Some(Token {
//			kind: TokenKind::Eof,
//			span: _
//		})
//	);
//}
//
//#[test]
//fn string_escape_hex_two_digits()
//{
//	let mut lexer: BasicLexer<'_> = BasicLexer::new(r#" "\x41" "#, SourceIndex::DUMMY);
//	assert_matches!(lexer.next(), Some(Token { kind: TokenKind::StringLiteral { string, flags: _ }, span: _ }) if string == "A");
//	assert_matches!(
//		lexer.next(),
//		Some(Token {
//			kind: TokenKind::Eof,
//			span: _
//		})
//	);
//}
//
//#[test]
//fn string_escape_hex_one_digit()
//{
//	// only one hex digit present before end of input -> hex_str = "9"
//	let mut lexer: BasicLexer<'_> = BasicLexer::new(r#" "\x9" "#, SourceIndex::DUMMY);
//	assert_matches!(lexer.next(), Some(Token { kind: TokenKind::StringLiteral { string, flags: _ }, span: _ }) if string == "\u{9}");
//	assert_matches!(
//		lexer.next(),
//		Some(Token {
//			kind: TokenKind::Eof,
//			span: _
//		})
//	);
//}
//
//#[test]
//fn string_escape_hex_max_byte_value()
//{
//	let mut lexer: BasicLexer<'_> = BasicLexer::new(r#" "\xff" "#, SourceIndex::DUMMY);
//	assert_matches!(lexer.next(), Some(Token { kind: TokenKind::StringLiteral { string, flags: _ }, span: _ }) if string == "\u{ff}");
//	assert_matches!(
//		lexer.next(),
//		Some(Token {
//			kind: TokenKind::Eof,
//			span: _
//		})
//	);
//}
//
//#[test]
//fn string_escape_hex_embedded_in_text()
//{
//	let mut lexer: BasicLexer<'_> = BasicLexer::new(r#" "a\x41b" "#, SourceIndex::DUMMY);
//	assert_matches!(lexer.next(), Some(Token { kind: TokenKind::StringLiteral { string, flags: _ }, span: _ }) if string == "aAb");
//	assert_matches!(
//		lexer.next(),
//		Some(Token {
//			kind: TokenKind::Eof,
//			span: _
//		})
//	);
//}
//
//#[test]
//fn string_escape_unicode_basic()
//{
//	let mut lexer: BasicLexer<'_> = BasicLexer::new(r#" "\u{41}" "#, SourceIndex::DUMMY);
//	assert_matches!(lexer.next(), Some(Token { kind: TokenKind::StringLiteral { string, flags: _ }, span: _ }) if string == "A");
//	assert_matches!(
//		lexer.next(),
//		Some(Token {
//			kind: TokenKind::Eof,
//			span: _
//		})
//	);
//}
//
//#[test]
//fn string_escape_unicode_single_digit()
//{
//	let mut lexer: BasicLexer<'_> = BasicLexer::new(r#" "\u{9}" "#, SourceIndex::DUMMY);
//	assert_matches!(lexer.next(), Some(Token { kind: TokenKind::StringLiteral { string, flags: _ }, span: _ }) if string == "\u{9}");
//	assert_matches!(
//		lexer.next(),
//		Some(Token {
//			kind: TokenKind::Eof,
//			span: _
//		})
//	);
//}
//
//#[test]
//fn string_escape_unicode_supplementary_plane()
//{
//	// emoji, requires a surrogate pair in UTF-16 but is a single char in UTF-8/Rust
//	let mut lexer: BasicLexer<'_> = BasicLexer::new(r#" "\u{1F600}" "#, SourceIndex::DUMMY);
//	assert_matches!(lexer.next(), Some(Token { kind: TokenKind::StringLiteral { string, flags: _ }, span: _ }) if string == "\u{1F600}");
//	assert_matches!(
//		lexer.next(),
//		Some(Token {
//			kind: TokenKind::Eof,
//			span: _
//		})
//	);
//}
//
//#[test]
//fn string_escape_unicode_max_length_six_digits()
//{
//	let mut lexer: BasicLexer<'_> = BasicLexer::new(r#" "\u{10FFFF}" "#, SourceIndex::DUMMY);
//	assert_matches!(lexer.next(), Some(Token { kind: TokenKind::StringLiteral { string, flags: _ }, span: _ }) if string == "\u{10FFFF}");
//	assert_matches!(
//		lexer.next(),
//		Some(Token {
//			kind: TokenKind::Eof,
//			span: _
//		})
//	);
//}
//
//#[test]
//fn string_escape_unicode_embedded_in_text()
//{
//	let mut lexer: BasicLexer<'_> = BasicLexer::new(r#" "a\u{41}b" "#, SourceIndex::DUMMY);
//	assert_matches!(lexer.next(), Some(Token { kind: TokenKind::StringLiteral { string, flags: _ }, span: _ }) if string == "aAb");
//	assert_matches!(
//		lexer.next(),
//		Some(Token {
//			kind: TokenKind::Eof,
//			span: _
//		})
//	);
//}

#[test]
fn string_unterminated()
{
	let mut lexer: BasicLexer<'_> = BasicLexer::new(r#" "test "#, SourceIndex::DUMMY);
	assert_matches!(
		lexer.next(),
		Some(Token {
			kind: TokenKind::Diag(diag),
			span: _
		})
		if diag.severity() == DiagnosticLevel::Error
	);

	assert_matches!(
		lexer.next(),
		Some(Token {
			kind: TokenKind::Invalid,
			span: _
		})
	);
	assert_matches!(
		lexer.next(),
		Some(Token {
			kind: TokenKind::Eof,
			span: _
		})
	);
}

#[test]
fn char_normal()
{
	let mut lexer: BasicLexer<'_> = BasicLexer::new(r#" 'a' "#, SourceIndex::DUMMY);
	assert_matches!(lexer.next(), Some(Token { kind: TokenKind::CharLiteral(c), span: _ }) if c == 'a');
	assert_matches!(
		lexer.next(),
		Some(Token {
			kind: TokenKind::Eof,
			span: _
		})
	);
}

#[test]
fn char_digit_start()
{
	let mut lexer: BasicLexer<'_> = BasicLexer::new(r#" '1' "#, SourceIndex::DUMMY);
	assert_matches!(lexer.next(), Some(Token { kind: TokenKind::CharLiteral(c), span: _ }) if c == '1');
	assert_matches!(
		lexer.next(),
		Some(Token {
			kind: TokenKind::Eof,
			span: _
		})
	);
}

#[test]
fn char_underscore_disambiguated_from_label()
{
	let mut lexer: BasicLexer<'_> = BasicLexer::new(r#" '_' "#, SourceIndex::DUMMY);
	assert_matches!(lexer.next(), Some(Token { kind: TokenKind::CharLiteral(c), span: _ }) if c == '_');
	assert_matches!(
		lexer.next(),
		Some(Token {
			kind: TokenKind::Eof,
			span: _
		})
	);
}

#[test]
fn char_escape_n()
{
	let mut lexer: BasicLexer<'_> = BasicLexer::new(r#" '\n' "#, SourceIndex::DUMMY);
	assert_matches!(lexer.next(), Some(Token { kind: TokenKind::CharLiteral(c), span: _ }) if c == '\n');
	assert_matches!(
		lexer.next(),
		Some(Token {
			kind: TokenKind::Eof,
			span: _
		})
	);
}

#[test]
fn char_escape_t()
{
	let mut lexer: BasicLexer<'_> = BasicLexer::new(r#" '\t' "#, SourceIndex::DUMMY);
	assert_matches!(lexer.next(), Some(Token { kind: TokenKind::CharLiteral(c), span: _ }) if c == '\t');
	assert_matches!(
		lexer.next(),
		Some(Token {
			kind: TokenKind::Eof,
			span: _
		})
	);
}

#[test]
fn char_escape_r()
{
	let mut lexer: BasicLexer<'_> = BasicLexer::new(r#" '\r' "#, SourceIndex::DUMMY);
	assert_matches!(lexer.next(), Some(Token { kind: TokenKind::CharLiteral(c), span: _ }) if c == '\r');
	assert_matches!(
		lexer.next(),
		Some(Token {
			kind: TokenKind::Eof,
			span: _
		})
	);
}

#[test]
fn char_escape_0()
{
	let mut lexer: BasicLexer<'_> = BasicLexer::new(r#" '\0' "#, SourceIndex::DUMMY);
	assert_matches!(lexer.next(), Some(Token { kind: TokenKind::CharLiteral(c), span: _ }) if c == '\0');
	assert_matches!(
		lexer.next(),
		Some(Token {
			kind: TokenKind::Eof,
			span: _
		})
	);
}

#[test]
fn char_escape_backslash()
{
	let mut lexer: BasicLexer<'_> = BasicLexer::new(r#" '\\' "#, SourceIndex::DUMMY);
	assert_matches!(lexer.next(), Some(Token { kind: TokenKind::CharLiteral(c), span: _ }) if c == '\\');
	assert_matches!(
		lexer.next(),
		Some(Token {
			kind: TokenKind::Eof,
			span: _
		})
	);
}

#[test]
fn char_escape_single_quote()
{
	let mut lexer: BasicLexer<'_> = BasicLexer::new(r#" '\'' "#, SourceIndex::DUMMY);
	assert_matches!(lexer.next(), Some(Token { kind: TokenKind::CharLiteral(c), span: _ }) if c == '\'');
	assert_matches!(
		lexer.next(),
		Some(Token {
			kind: TokenKind::Eof,
			span: _
		})
	);
}

#[test]
fn char_escape_double_quote()
{
	let mut lexer: BasicLexer<'_> = BasicLexer::new(r#" '\"' "#, SourceIndex::DUMMY);
	assert_matches!(lexer.next(), Some(Token { kind: TokenKind::CharLiteral(c), span: _ }) if c == '"');
	assert_matches!(
		lexer.next(),
		Some(Token {
			kind: TokenKind::Eof,
			span: _
		})
	);
}

#[test]
fn char_escape_hex_two_digits()
{
	let mut lexer: BasicLexer<'_> = BasicLexer::new(r#" '\x41' "#, SourceIndex::DUMMY);
	assert_matches!(lexer.next(), Some(Token { kind: TokenKind::CharLiteral(c), span: _ }) if c == 'A');
	assert_matches!(
		lexer.next(),
		Some(Token {
			kind: TokenKind::Eof,
			span: _
		})
	);
}

#[test]
fn char_escape_hex_one_digit()
{
	let mut lexer: BasicLexer<'_> = BasicLexer::new(r#" '\x9' "#, SourceIndex::DUMMY);
	assert_matches!(lexer.next(), Some(Token { kind: TokenKind::CharLiteral(c), span: _ }) if c == '\u{9}');
	assert_matches!(
		lexer.next(),
		Some(Token {
			kind: TokenKind::Eof,
			span: _
		})
	);
}

#[test]
fn char_escape_hex_max_byte_value()
{
	let mut lexer: BasicLexer<'_> = BasicLexer::new(r#" '\xff' "#, SourceIndex::DUMMY);
	assert_matches!(lexer.next(), Some(Token { kind: TokenKind::CharLiteral(c), span: _ }) if c == '\u{ff}');
	assert_matches!(
		lexer.next(),
		Some(Token {
			kind: TokenKind::Eof,
			span: _
		})
	);
}

#[test]
fn char_escape_unicode_basic()
{
	let mut lexer: BasicLexer<'_> = BasicLexer::new(r#" '\u{41}' "#, SourceIndex::DUMMY);
	assert_matches!(lexer.next(), Some(Token { kind: TokenKind::CharLiteral(c), span: _ }) if c == 'A');
	assert_matches!(
		lexer.next(),
		Some(Token {
			kind: TokenKind::Eof,
			span: _
		})
	);
}

#[test]
fn char_escape_unicode_single_digit()
{
	let mut lexer: BasicLexer<'_> = BasicLexer::new(r#" '\u{9}' "#, SourceIndex::DUMMY);
	assert_matches!(lexer.next(), Some(Token { kind: TokenKind::CharLiteral(c), span: _ }) if c == '\u{9}');
	assert_matches!(
		lexer.next(),
		Some(Token {
			kind: TokenKind::Eof,
			span: _
		})
	);
}

#[test]
fn char_escape_unicode_supplementary_plane()
{
	// emoji, requires a surrogate pair in UTF-16 but is a single char in UTF-8/Rust
	let mut lexer: BasicLexer<'_> = BasicLexer::new(r#" '\u{1F600}' "#, SourceIndex::DUMMY);
	assert_matches!(lexer.next(), Some(Token { kind: TokenKind::CharLiteral(c), span: _ }) if c == '\u{1F600}');
	assert_matches!(
		lexer.next(),
		Some(Token {
			kind: TokenKind::Eof,
			span: _
		})
	);
}

#[test]
fn char_escape_unicode_max_length_six_digits()
{
	let mut lexer: BasicLexer<'_> = BasicLexer::new(r#" '\u{10FFFF}' "#, SourceIndex::DUMMY);
	assert_matches!(lexer.next(), Some(Token { kind: TokenKind::CharLiteral(c), span: _ }) if c == '\u{10FFFF}');
	assert_matches!(
		lexer.next(),
		Some(Token {
			kind: TokenKind::Eof,
			span: _
		})
	);
}

#[test]
fn char_invalid_escape()
{
	let mut lexer: BasicLexer<'_> = BasicLexer::new(r#" '\q' "#, SourceIndex::DUMMY);
	assert_matches!(
		lexer.next(),
		Some(Token {
			kind: TokenKind::Diag(diag),
			span: _
		})
		if diag.severity() == DiagnosticLevel::Error
	);

	assert_matches!(
		lexer.next(),
		Some(Token {
			kind: TokenKind::Invalid,
			span: _
		})
	);
	assert_matches!(
		lexer.next(),
		Some(Token {
			kind: TokenKind::Eof,
			span: _
		})
	);
}

#[test]
fn char_unterminated_no_closing_quote()
{
	let mut lexer: BasicLexer<'_> = BasicLexer::new(r#" '1"#, SourceIndex::DUMMY);
	assert_matches!(
		lexer.next(),
		Some(Token {
			kind: TokenKind::Diag(diag),
			span: _
		})
		if diag.severity() == DiagnosticLevel::Error
	);

	assert_matches!(
		lexer.next(),
		Some(Token {
			kind: TokenKind::Invalid,
			span: _
		})
	);
	assert_matches!(
		lexer.next(),
		Some(Token {
			kind: TokenKind::Eof,
			span: _
		})
	);
}

#[test]
fn char_unterminated_empty_at_eof()
{
	let mut lexer: BasicLexer<'_> = BasicLexer::new(r#" '"#, SourceIndex::DUMMY);
	assert_matches!(
		lexer.next(),
		Some(Token {
			kind: TokenKind::Diag(diag),
			span: _
		})
		if diag.severity() == DiagnosticLevel::Error
	);

	assert_matches!(
		lexer.next(),
		Some(Token {
			kind: TokenKind::Invalid,
			span: _
		})
	);
	assert_matches!(
		lexer.next(),
		Some(Token {
			kind: TokenKind::Eof,
			span: _
		})
	);
}

#[test]
fn char_double_single_quote()
{
	let mut lexer: BasicLexer<'_> = BasicLexer::new(r#" '' "#, SourceIndex::DUMMY);
	assert_matches!(
		lexer.next(),
		Some(Token {
			kind: TokenKind::Diag(diag),
			span: _
		})
		if diag.severity() == DiagnosticLevel::Error
	);

	assert_matches!(
		lexer.next(),
		Some(Token {
			kind: TokenKind::Invalid,
			span: _
		})
	);
}

#[test]
fn label_simple()
{
	let mut lexer: BasicLexer<'_> = BasicLexer::new(r#" 'label "#, SourceIndex::DUMMY);
	assert_matches!(lexer.next(), Some(Token { kind: TokenKind::Label(label), span: _ }) if label == "'label");
	assert_matches!(
		lexer.next(),
		Some(Token {
			kind: TokenKind::Eof,
			span: _
		})
	);
}

#[test]
fn label_underscore_start()
{
	let mut lexer: BasicLexer<'_> = BasicLexer::new(r#" '_foo "#, SourceIndex::DUMMY);
	assert_matches!(lexer.next(), Some(Token { kind: TokenKind::Label(label), span: _ }) if label == "'_foo");
	assert_matches!(
		lexer.next(),
		Some(Token {
			kind: TokenKind::Eof,
			span: _
		})
	);
}

#[test]
fn label_alphanumeric()
{
	let mut lexer: BasicLexer<'_> = BasicLexer::new(r#" 'label123 "#, SourceIndex::DUMMY);
	assert_matches!(lexer.next(), Some(Token { kind: TokenKind::Label(label), span: _ }) if label == "'label123");
	assert_matches!(
		lexer.next(),
		Some(Token {
			kind: TokenKind::Eof,
			span: _
		})
	);
}

#[test]
fn label_single_char_followed_by_space()
{
	let mut lexer: BasicLexer<'_> = BasicLexer::new(r#" 'a "#, SourceIndex::DUMMY);
	assert_matches!(lexer.next(), Some(Token { kind: TokenKind::Label(label), span: _ }) if label == "'a");
	assert_matches!(
		lexer.next(),
		Some(Token {
			kind: TokenKind::Eof,
			span: _
		})
	);
}

#[test]
fn label_single_char_followed_by_punctuation()
{
	let mut lexer: BasicLexer<'_> = BasicLexer::new(r#" 'a; "#, SourceIndex::DUMMY);
	assert_matches!(lexer.next(), Some(Token { kind: TokenKind::Label(label), span: _ }) if label == "'a");
	assert_matches!(
		lexer.next(),
		Some(Token {
			kind: TokenKind::Semicolon,
			span: _
		})
	);
	assert_matches!(
		lexer.next(),
		Some(Token {
			kind: TokenKind::Eof,
			span: _
		})
	);
}

#[test]
fn label_single_underscore_followed_by_space()
{
	let mut lexer: BasicLexer<'_> = BasicLexer::new(r#" '_ "#, SourceIndex::DUMMY);
	assert_matches!(lexer.next(), Some(Token { kind: TokenKind::Label(label), span: _ }) if label == "'_");
	assert_matches!(
		lexer.next(),
		Some(Token {
			kind: TokenKind::Eof,
			span: _
		})
	);
}

#[test]
fn label_at_eof()
{
	let mut lexer: BasicLexer<'_> = BasicLexer::new(r#" 'outer"#, SourceIndex::DUMMY);
	assert_matches!(lexer.next(), Some(Token { kind: TokenKind::Label(label), span: _ }) if label == "'outer");
	assert_matches!(
		lexer.next(),
		Some(Token {
			kind: TokenKind::Eof,
			span: _
		})
	);
}

#[test]
fn label_used_as_loop_label()
{
	let mut lexer: BasicLexer<'_> = BasicLexer::new(r#" 'outer: while "#, SourceIndex::DUMMY);
	assert_matches!(lexer.next(), Some(Token { kind: TokenKind::Label(label), span: _ }) if label == "'outer");
	assert_matches!(
		lexer.next(),
		Some(Token {
			kind: TokenKind::Colon,
			span: _
		})
	);
	assert_matches!(
		lexer.next(),
		Some(Token {
			kind: TokenKind::While,
			span: _
		})
	);
	assert_matches!(
		lexer.next(),
		Some(Token {
			kind: TokenKind::Eof,
			span: _
		})
	);
}

#[test]
fn int_decimal_simple()
{
	let mut lexer: BasicLexer<'_> = BasicLexer::new(r#" 42 "#, SourceIndex::DUMMY);
	assert_matches!(
		lexer.next(),
		Some(Token { kind: TokenKind::IntLiteral { value, base: IntBase::Decimal, ty: None }, span: _ })
		if value == "42"
	);
	assert_matches!(
		lexer.next(),
		Some(Token {
			kind: TokenKind::Eof,
			span: _
		})
	);
}

#[test]
fn int_decimal_zero()
{
	let mut lexer: BasicLexer<'_> = BasicLexer::new(r#" 0 "#, SourceIndex::DUMMY);
	assert_matches!(
		lexer.next(),
		Some(Token { kind: TokenKind::IntLiteral { value, base: IntBase::Decimal, ty: None }, span: _ })
		if value == "0"
	);
	assert_matches!(
		lexer.next(),
		Some(Token {
			kind: TokenKind::Eof,
			span: _
		})
	);
}

#[test]
fn int_dot_not_followed_by_digit_is_not_a_float()
{
	let mut lexer: BasicLexer<'_> = BasicLexer::new(r#" 3. "#, SourceIndex::DUMMY);
	assert_matches!(
		lexer.next(),
		Some(Token { kind: TokenKind::IntLiteral { value, base: IntBase::Decimal, ty: None }, span: _ })
		if value == "3"
	);
	assert_matches!(
		lexer.next(),
		Some(Token {
			kind: TokenKind::Dot,
			span: _
		})
	);
	assert_matches!(
		lexer.next(),
		Some(Token {
			kind: TokenKind::Eof,
			span: _
		})
	);
}

// #[test]
// fn int_decimal_underscore_regular_grouping()
// {
// 	let mut lexer: BasicLexer<'_> = BasicLexer::new(r#" 1_000_000 "#, SourceIndex::DUMMY);
// 	assert_matches!(
// 		lexer.next(),
// 		Some(Token { kind: TokenKind::IntLiteral { value, base: IntBase::Decimal, ty: None }, span: _ })
// 		if value == "1000000"
// 	);
// 	assert_matches!(
// 		lexer.next(),
// 		Some(Token {
// 			kind: TokenKind::Eof,
// 			span: _
// 		})
// 	);
// }
//
// #[test]
// fn int_decimal_underscore_irregular_grouping()
// {
// 	let mut lexer: BasicLexer<'_> = BasicLexer::new(r#" 1_00_000 "#, SourceIndex::DUMMY);
// 	assert_matches!(
// 		lexer.next(),
// 		Some(Token {
// 			kind: TokenKind::Diag(diag),
// 			span: _
// 		})
// 		if diag.severity() == DiagnosticLevel::Warning
// 	);
//
// 	assert_matches!(
// 		lexer.next(),
// 		Some(Token { kind: TokenKind::IntLiteral { value, base: IntBase::Decimal, ty: None }, span: _ })
// 		if value == "100000"
// 	);
// 	assert_matches!(
// 		lexer.next(),
// 		Some(Token {
// 			kind: TokenKind::Eof,
// 			span: _
// 		})
// 	);
// }

#[test]
fn int_suffix_unsigned_fixed()
{
	let mut lexer: BasicLexer<'_> = BasicLexer::new(r#" 42u32 "#, SourceIndex::DUMMY);
	assert_matches!(
		lexer.next(),
		Some(Token {
			kind: TokenKind::IntLiteral {
				value,
				base: IntBase::Decimal,
				ty: Some(IntType { bits: IntSize::Fixed(32), sign: IntSign::Unsigned }),
			},
			span: _,
		})
		if value == "42"
	);
	assert_matches!(
		lexer.next(),
		Some(Token {
			kind: TokenKind::Eof,
			span: _
		})
	);
}

#[test]
fn int_suffix_signed_fixed()
{
	let mut lexer: BasicLexer<'_> = BasicLexer::new(r#" 42i64 "#, SourceIndex::DUMMY);
	assert_matches!(
		lexer.next(),
		Some(Token {
			kind: TokenKind::IntLiteral {
				value,
				base: IntBase::Decimal,
				ty: Some(IntType { bits: IntSize::Fixed(64), sign: IntSign::Signed }),
			},
			span: _,
		})
		if value == "42"
	);
	assert_matches!(
		lexer.next(),
		Some(Token {
			kind: TokenKind::Eof,
			span: _
		})
	);
}

#[test]
fn int_suffix_unsigned_size()
{
	let mut lexer: BasicLexer<'_> = BasicLexer::new(r#" 42usize "#, SourceIndex::DUMMY);
	assert_matches!(
		lexer.next(),
		Some(Token {
			kind: TokenKind::IntLiteral {
				value,
				base: IntBase::Decimal,
				ty: Some(IntType { bits: IntSize::Size, sign: IntSign::Unsigned }),
			},
			span: _,
		})
		if value == "42"
	);
	assert_matches!(
		lexer.next(),
		Some(Token {
			kind: TokenKind::Eof,
			span: _
		})
	);
}

#[test]
fn int_suffix_signed_size()
{
	let mut lexer: BasicLexer<'_> = BasicLexer::new(r#" 42isize "#, SourceIndex::DUMMY);
	assert_matches!(
		lexer.next(),
		Some(Token {
			kind: TokenKind::IntLiteral {
				value,
				base: IntBase::Decimal,
				ty: Some(IntType { bits: IntSize::Size, sign: IntSign::Signed }),
			},
			span: _,
		})
		if value == "42"
	);
	assert_matches!(
		lexer.next(),
		Some(Token {
			kind: TokenKind::Eof,
			span: _
		})
	);
}

#[test]
fn int_suffix_missing_digits_backtracks()
{
	let mut lexer: BasicLexer<'_> = BasicLexer::new(r#" 42u "#, SourceIndex::DUMMY);

	assert_matches!(
		lexer.next(),
		Some(Token {
			kind: TokenKind::Diag(diag),
			span: _
		})
		if diag.severity() == DiagnosticLevel::Warning
	);
	assert_matches!(
		lexer.next(),
		Some(Token { kind: TokenKind::IntLiteral { value, base: IntBase::Decimal, ty: None }, span: _ })
		if value == "42"
	);
	assert_matches!(lexer.next(), Some(Token { kind: TokenKind::Identifier(ident), span: _ }) if ident == "u");
	assert_matches!(
		lexer.next(),
		Some(Token {
			kind: TokenKind::Eof,
			span: _
		})
	);
}

#[test]
fn int_suffix_malformed_size_word_backtracks()
{
	let mut lexer: BasicLexer<'_> = BasicLexer::new(r#" 42is "#, SourceIndex::DUMMY);

	assert_matches!(
		lexer.next(),
		Some(Token {
			kind: TokenKind::Diag(diag),
			span: _
		})
		if diag.severity() == DiagnosticLevel::Warning
	);
	assert_matches!(
		lexer.next(),
		Some(Token { kind: TokenKind::IntLiteral { value, base: IntBase::Decimal, ty: None }, span: _ })
		if value == "42"
	);
	assert_matches!(lexer.next(), Some(Token { kind: TokenKind::Identifier(ident), span: _ }) if ident == "is");
	assert_matches!(
		lexer.next(),
		Some(Token {
			kind: TokenKind::Eof,
			span: _
		})
	);
}

#[test]
fn int_hex()
{
	let mut lexer: BasicLexer<'_> = BasicLexer::new(r#" 0x1A "#, SourceIndex::DUMMY);
	assert_matches!(
		lexer.next(),
		Some(Token { kind: TokenKind::IntLiteral { value, base: IntBase::Hexadecimal, ty: None }, span: _ })
		if value == "1A"
	);
	assert_matches!(
		lexer.next(),
		Some(Token {
			kind: TokenKind::Eof,
			span: _
		})
	);
}

#[test]
fn int_hex_lowercase()
{
	let mut lexer: BasicLexer<'_> = BasicLexer::new(r#" 0xff "#, SourceIndex::DUMMY);
	assert_matches!(
		lexer.next(),
		Some(Token { kind: TokenKind::IntLiteral { value, base: IntBase::Hexadecimal, ty: None }, span: _ })
		if value == "ff"
	);
	assert_matches!(
		lexer.next(),
		Some(Token {
			kind: TokenKind::Eof,
			span: _
		})
	);
}

#[test]
fn int_hex_with_suffix()
{
	let mut lexer: BasicLexer<'_> = BasicLexer::new(r#" 0xFFu8 "#, SourceIndex::DUMMY);
	assert_matches!(
		lexer.next(),
		Some(Token {
			kind: TokenKind::IntLiteral {
				value,
				base: IntBase::Hexadecimal,
				ty: Some(IntType { bits: IntSize::Fixed(8), sign: IntSign::Unsigned }),
			},
			span: _,
		})
		if value == "FF"
	);
	assert_matches!(
		lexer.next(),
		Some(Token {
			kind: TokenKind::Eof,
			span: _
		})
	);
}

//#[test]
//fn int_hex_with_regular_underscore()
//{
//	let mut lexer: BasicLexer<'_> = BasicLexer::new(r#" 0xFF_FF "#, SourceIndex::DUMMY);
//	assert_matches!(
//		lexer.next(),
//		Some(Token { kind: TokenKind::IntLiteral { value, base: IntBase::Hexadecimal, ty: None }, span: _ })
//		if value == "FFFF"
//	);
//	assert_matches!(
//		lexer.next(),
//		Some(Token {
//			kind: TokenKind::Eof,
//			span: _
//		})
//	);
//}

#[test]
fn int_binary()
{
	let mut lexer: BasicLexer<'_> = BasicLexer::new(r#" 0b1010 "#, SourceIndex::DUMMY);
	assert_matches!(
		lexer.next(),
		Some(Token { kind: TokenKind::IntLiteral { value, base: IntBase::Binary, ty: None }, span: _ })
		if value == "1010"
	);
	assert_matches!(
		lexer.next(),
		Some(Token {
			kind: TokenKind::Eof,
			span: _
		})
	);
}

#[test]
fn int_binary_stops_at_invalid_digit()
{
	let mut lexer: BasicLexer<'_> = BasicLexer::new(r#" 0b1012 "#, SourceIndex::DUMMY);
	assert_matches!(
		lexer.next(),
		Some(Token {
			kind: TokenKind::Diag(diag),
			span: _
		})
		if diag.severity() == DiagnosticLevel::Warning
	);
	assert_matches!(
		lexer.next(),
		Some(Token { kind: TokenKind::IntLiteral { value, base: IntBase::Binary, ty: None }, span: _ })
		if value == "101"
	);
	assert_matches!(
		lexer.next(),
		Some(Token { kind: TokenKind::IntLiteral { value, base: IntBase::Decimal, ty: None }, span: _ })
		if value == "2"
	);
	assert_matches!(
		lexer.next(),
		Some(Token {
			kind: TokenKind::Eof,
			span: _
		})
	);
}

#[test]
fn int_octal()
{
	let mut lexer: BasicLexer<'_> = BasicLexer::new(r#" 0o17 "#, SourceIndex::DUMMY);
	assert_matches!(
		lexer.next(),
		Some(Token { kind: TokenKind::IntLiteral { value, base: IntBase::Octal, ty: None }, span: _ })
		if value == "17"
	);
	assert_matches!(
		lexer.next(),
		Some(Token {
			kind: TokenKind::Eof,
			span: _
		})
	);
}

#[test]
fn int_octal_stops_at_invalid_digit()
{
	let mut lexer: BasicLexer<'_> = BasicLexer::new(r#" 0o178 "#, SourceIndex::DUMMY);

	assert_matches!(
		lexer.next(),
		Some(Token {
			kind: TokenKind::Diag(diag),
			span: _
		})
		if diag.severity() == DiagnosticLevel::Warning
	);
	assert_matches!(
		lexer.next(),
		Some(Token { kind: TokenKind::IntLiteral { value, base: IntBase::Octal, ty: None }, span: _ })
		if value == "17"
	);
	assert_matches!(
		lexer.next(),
		Some(Token { kind: TokenKind::IntLiteral { value, base: IntBase::Decimal, ty: None }, span: _ })
		if value == "8"
	);
	assert_matches!(
		lexer.next(),
		Some(Token {
			kind: TokenKind::Eof,
			span: _
		})
	);
}

#[test]
fn float_simple()
{
	let mut lexer: BasicLexer<'_> = BasicLexer::new(r#" 3.14 "#, SourceIndex::DUMMY);
	assert_matches!(
		lexer.next(),
		Some(Token { kind: TokenKind::FloatLiteral { value, bits: None }, span: _ })
		if value == "3.14"
	);
	assert_matches!(
		lexer.next(),
		Some(Token {
			kind: TokenKind::Eof,
			span: _
		})
	);
}

#[test]
fn float_with_suffix()
{
	let mut lexer: BasicLexer<'_> = BasicLexer::new(r#" 3.14f32 "#, SourceIndex::DUMMY);
	assert_matches!(
		lexer.next(),
		Some(Token { kind: TokenKind::FloatLiteral { value, bits: Some(32) }, span: _ })
		if value == "3.14"
	);
	assert_matches!(
		lexer.next(),
		Some(Token {
			kind: TokenKind::Eof,
			span: _
		})
	);
}

#[test]
fn float_suffix_f_with_no_digits()
{
	let mut lexer: BasicLexer<'_> = BasicLexer::new(r#" 3.14f "#, SourceIndex::DUMMY);
	assert_matches!(
		lexer.next(),
		Some(Token { kind: TokenKind::FloatLiteral { value, bits: None }, span: _ })
		if value == "3.14"
	);
	assert_matches!(
		lexer.next(),
		Some(Token {
			kind: TokenKind::Eof,
			span: _
		})
	);
}

// #[test]
// fn float_underscore_regular_grouping()
// {
// 	let mut lexer: BasicLexer<'_> = BasicLexer::new(r#" 1_000.5 "#, SourceIndex::DUMMY);
// 	assert_matches!(
// 		lexer.next(),
// 		Some(Token { kind: TokenKind::FloatLiteral { value, bits: None }, span: _ })
// 		if value == "1000.5"
// 	);
// 	assert_matches!(
// 		lexer.next(),
// 		Some(Token {
// 			kind: TokenKind::Eof,
// 			span: _
// 		})
// 	);
// }
//
// #[test]
// fn float_underscore_irregular_grouping()
// {
// 	let mut lexer: BasicLexer<'_> = BasicLexer::new(r#" 12_3.45 "#, SourceIndex::DUMMY);
// 	assert_matches!(
// 		lexer.next(),
// 		Some(Token {
// 			kind: TokenKind::Diag(diag),
// 			span: _
// 		})
// 		if diag.severity() == DiagnosticLevel::Warning
// 	);
//
// 	assert_matches!(
// 		lexer.next(),
// 		Some(Token { kind: TokenKind::FloatLiteral { value, bits: None }, span: _ })
// 		if value == "123.45"
// 	);
// 	assert_matches!(
// 		lexer.next(),
// 		Some(Token {
// 			kind: TokenKind::Eof,
// 			span: _
// 		})
// 	);
// }

#[test]
fn float_then_semicolon()
{
	let mut lexer: BasicLexer<'_> = BasicLexer::new(r#" 3.14; "#, SourceIndex::DUMMY);
	assert_matches!(
		lexer.next(),
		Some(Token { kind: TokenKind::FloatLiteral { value, bits: None }, span: _ })
		if value == "3.14"
	);
	assert_matches!(
		lexer.next(),
		Some(Token {
			kind: TokenKind::Semicolon,
			span: _
		})
	);
	assert_matches!(
		lexer.next(),
		Some(Token {
			kind: TokenKind::Eof,
			span: _
		})
	);
}
