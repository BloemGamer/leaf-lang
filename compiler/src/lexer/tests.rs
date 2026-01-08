#[cfg(test)]
#[allow(clippy::module_inception)]
mod tests
{
	use crate::lexer::*;

	/// Helper function to extract token kinds from source
	fn lex_kinds(source: &str) -> Vec<TokenKind>
	{
		let config = Config::default();
		let mut source_map = SourceMap::default();
let lexer = Lexer::new_add_to_source_map(&config, source, "test_file_1", &mut source_map);
		return lexer.map(|t| return t.kind).collect();
	}

	/// Helper function to extract tokens from source
	fn lex_tokens(source: &str) -> Vec<Token>
	{
		let config = Config::default();
		let mut source_map = SourceMap::default();
let lexer = Lexer::new_add_to_source_map(&config, source, "test_file_2", &mut source_map);
		return lexer.collect();
	}

	// ===== Basic Token Tests =====

	#[test]
	fn test_basic_tokens()
	{
		let source = "var x = 42 + 3.14;";
		let config = Config::default();
		let mut source_map = SourceMap::default();
let lexer = Lexer::new_add_to_source_map(&config, source, "test_file_3", &mut source_map);

		assert_eq!(lexer.into_iter().count(), 8);
	}

	#[test]
	fn test_empty_source()
	{
		let kinds = lex_kinds("");
		assert_eq!(kinds, vec![TokenKind::Eof]);
	}

	#[test]
	fn test_whitespace_only()
	{
		let kinds = lex_kinds("   \n\t  \r\n  ");
		assert_eq!(kinds, vec![TokenKind::Eof]);
	}

	// ===== Literal Tests =====

	#[test]
	fn test_integer_literals()
	{
		let kinds = lex_kinds("0 42 123 1_000_000");
		assert_eq!(
			kinds,
			vec![
				TokenKind::IntLiteral(0),
				TokenKind::IntLiteral(42),
				TokenKind::IntLiteral(123),
				TokenKind::IntLiteral(1000000),
				TokenKind::Eof,
			]
		);
	}

	#[test]
	fn test_hex_literals()
	{
		let kinds = lex_kinds("0x0 0xFF 0xDEADBEEF 0x1_2_3");
		assert_eq!(
			kinds,
			vec![
				TokenKind::IntLiteral(0),
				TokenKind::IntLiteral(255),
				TokenKind::IntLiteral(0xDEADBEEF),
				TokenKind::IntLiteral(0x123),
				TokenKind::Eof,
			]
		);
	}

	#[test]
	fn test_binary_literals()
	{
		let kinds = lex_kinds("0b0 0b1010 0b1111_0000");
		assert_eq!(
			kinds,
			vec![
				TokenKind::IntLiteral(0),
				TokenKind::IntLiteral(0b1010),
				TokenKind::IntLiteral(0b11110000),
				TokenKind::Eof,
			]
		);
	}

	#[test]
	fn test_octal_literals()
	{
		let kinds = lex_kinds("0o0 0o777 0o1_2_3");
		assert_eq!(
			kinds,
			vec![
				TokenKind::IntLiteral(0),
				TokenKind::IntLiteral(0o777),
				TokenKind::IntLiteral(0o123),
				TokenKind::Eof,
			]
		);
	}

	#[test]
	fn test_float_literals()
	{
		let kinds = lex_kinds("3.12 0.5 123.456 1_000.5_0");
		assert_eq!(
			kinds,
			vec![
				TokenKind::FloatLiteral(3.12),
				TokenKind::FloatLiteral(0.5),
				TokenKind::FloatLiteral(123.456),
				TokenKind::FloatLiteral(1000.50),
				TokenKind::Eof,
			]
		);
	}

	#[test]
	fn test_string_literals()
	{
		let kinds = lex_kinds(r#""hello" "world" "" "with spaces""#);
		assert_eq!(
			kinds,
			vec![
				TokenKind::StringLiteral("hello".to_string()),
				TokenKind::StringLiteral("world".to_string()),
				TokenKind::StringLiteral("".to_string()),
				TokenKind::StringLiteral("with spaces".to_string()),
				TokenKind::Eof,
			]
		);
	}

	#[test]
	fn test_string_escape_sequences()
	{
		let kinds = lex_kinds(r#""hello\nworld" "tab\there" "quote\"" "backslash\\""#);
		assert_eq!(
			kinds,
			vec![
				TokenKind::StringLiteral("hello\nworld".to_string()),
				TokenKind::StringLiteral("tab\there".to_string()),
				TokenKind::StringLiteral("quote\"".to_string()),
				TokenKind::StringLiteral("backslash\\".to_string()),
				TokenKind::Eof,
			]
		);
	}

	#[test]
	fn test_char_literals()
	{
		let kinds = lex_kinds(r"'a' 'Z' '0' ' '");
		assert_eq!(
			kinds,
			vec![
				TokenKind::CharLiteral('a'),
				TokenKind::CharLiteral('Z'),
				TokenKind::CharLiteral('0'),
				TokenKind::CharLiteral(' '),
				TokenKind::Eof,
			]
		);
	}

	#[test]
	fn test_char_escape_sequences()
	{
		let kinds = lex_kinds(r"'\n' '\t' '\r' '\0' '\\' '\''");
		assert_eq!(
			kinds,
			vec![
				TokenKind::CharLiteral('\n'),
				TokenKind::CharLiteral('\t'),
				TokenKind::CharLiteral('\r'),
				TokenKind::CharLiteral('\0'),
				TokenKind::CharLiteral('\\'),
				TokenKind::CharLiteral('\''),
				TokenKind::Eof,
			]
		);
	}

	#[test]
	fn test_boolean_literals()
	{
		let kinds = lex_kinds("true false");
		assert_eq!(kinds, vec![TokenKind::True, TokenKind::False, TokenKind::Eof]);
	}

	// ===== Identifier and Keyword Tests =====

	#[test]
	fn test_identifiers()
	{
		let kinds = lex_kinds("foo bar my_var _private MyType");
		assert_eq!(
			kinds,
			vec![
				TokenKind::Identifier("foo".to_string()),
				TokenKind::Identifier("bar".to_string()),
				TokenKind::Identifier("my_var".to_string()),
				TokenKind::Identifier("_private".to_string()),
				TokenKind::Identifier("MyType".to_string()),
				TokenKind::Eof,
			]
		);
	}

	#[test]
	fn test_underscore_wildcard()
	{
		let kinds = lex_kinds("_");
		assert_eq!(kinds, vec![TokenKind::Underscore, TokenKind::Eof]);
	}

	#[test]
	fn test_control_flow_keywords()
	{
		let kinds = lex_kinds("if else while for switch return break continue");
		assert_eq!(
			kinds,
			vec![
				TokenKind::If,
				TokenKind::Else,
				TokenKind::While,
				TokenKind::For,
				TokenKind::Switch,
				TokenKind::Return,
				TokenKind::Break,
				TokenKind::Continue,
				TokenKind::Eof,
			]
		);
	}

	#[test]
	fn test_declaration_keywords()
	{
		let kinds = lex_kinds("fn const static struct union variant enum impl macro");
		assert_eq!(
			kinds,
			vec![
				TokenKind::FuncDef,
				TokenKind::Const,
				TokenKind::Static,
				TokenKind::Struct,
				TokenKind::Union,
				TokenKind::Variant,
				TokenKind::Enum,
				TokenKind::Impl,
				TokenKind::MacroDef,
				TokenKind::Eof,
			]
		);
	}

	#[test]
	fn test_modifier_keywords()
	{
		let kinds = lex_kinds("pub mut unsafe volatile");
		assert_eq!(
			kinds,
			vec![
				TokenKind::Pub,
				TokenKind::Mut,
				TokenKind::Unsafe,
				TokenKind::Volatile,
				TokenKind::Eof
			]
		);
	}

	#[test]
	fn test_other_keywords()
	{
		let kinds = lex_kinds("in as where");
		assert_eq!(
			kinds,
			vec![TokenKind::In, TokenKind::As, TokenKind::Where, TokenKind::Eof]
		);
	}

	// ===== Operator Tests =====

	#[test]
	fn test_arithmetic_operators()
	{
		let kinds = lex_kinds("+ - * / %");
		assert_eq!(
			kinds,
			vec![
				TokenKind::Plus,
				TokenKind::Minus,
				TokenKind::Star,
				TokenKind::Slash,
				TokenKind::Mod,
				TokenKind::Eof,
			]
		);
	}

	#[test]
	fn test_bitwise_operators()
	{
		let kinds = lex_kinds("| & ^ ~ << >>");
		assert_eq!(
			kinds,
			vec![
				TokenKind::Pipe,
				TokenKind::Ampersand,
				TokenKind::Caret,
				TokenKind::Tilde,
				TokenKind::LShift,
				TokenKind::RShift,
				TokenKind::Eof,
			]
		);
	}

	#[test]
	fn test_logical_operators()
	{
		let kinds = lex_kinds("! && ||");
		assert_eq!(
			kinds,
			vec![TokenKind::Bang, TokenKind::And, TokenKind::Or, TokenKind::Eof]
		);
	}

	#[test]
	fn test_comparison_operators()
	{
		let kinds = lex_kinds("< > <= >= == !=");
		assert_eq!(
			kinds,
			vec![
				TokenKind::LessThan,
				TokenKind::GreaterThan,
				TokenKind::LessEquals,
				TokenKind::GreaterEquals,
				TokenKind::EqualsEquals,
				TokenKind::BangEquals,
				TokenKind::Eof,
			]
		);
	}

	#[test]
	fn test_assignment_operators()
	{
		let kinds = lex_kinds("= += -= *= /= %= |= &= ^= ~= <<= >>=");
		assert_eq!(
			kinds,
			vec![
				TokenKind::Equals,
				TokenKind::PlusEquals,
				TokenKind::MinusEquals,
				TokenKind::StarEquals,
				TokenKind::SlashEquals,
				TokenKind::ModEquals,
				TokenKind::PipeEquals,
				TokenKind::AmpersandEquals,
				TokenKind::CaretEquals,
				TokenKind::TildeEquals,
				TokenKind::LShiftEquals,
				TokenKind::RShiftEquals,
				TokenKind::Eof,
			]
		);
	}

	#[test]
	fn test_arrow_operators()
	{
		let kinds = lex_kinds("-> =>");
		assert_eq!(kinds, vec![TokenKind::Arrow, TokenKind::FatArrow, TokenKind::Eof]);
	}

	// ===== Delimiter and Punctuation Tests =====

	#[test]
	fn test_delimiters()
	{
		let kinds = lex_kinds("( ) { } [ ]");
		assert_eq!(
			kinds,
			vec![
				TokenKind::LeftParen,
				TokenKind::RightParen,
				TokenKind::LeftBrace,
				TokenKind::RightBrace,
				TokenKind::LeftBracket,
				TokenKind::RightBracket,
				TokenKind::Eof,
			]
		);
	}

	#[test]
	fn test_punctuation()
	{
		let kinds = lex_kinds("; : :: , . .. ... ? # \\");
		assert_eq!(
			kinds,
			vec![
				TokenKind::Semicolon,
				TokenKind::Colon,
				TokenKind::DoubleColon,
				TokenKind::Comma,
				TokenKind::Dot,
				TokenKind::DotDot,
				TokenKind::Ellipsis,
				TokenKind::QuestionMark,
				TokenKind::Hash,
				TokenKind::Backslash,
				TokenKind::Eof,
			]
		);
	}

	// ===== Comment Tests =====

	#[test]
	fn test_line_comment()
	{
		let kinds = lex_kinds("// this is a comment\n42");
		assert_eq!(
			kinds,
			vec![
				// TokenKind::LineComment(" this is a comment".to_string()),
				TokenKind::IntLiteral(42),
				TokenKind::Eof,
			]
		);
	}

	#[test]
	fn test_doc_comment()
	{
		let kinds = lex_kinds("/// this is a doc comment\n42");
		assert_eq!(
			kinds,
			vec![
				TokenKind::DocsComment(" this is a doc comment".to_string()),
				TokenKind::IntLiteral(42),
				TokenKind::Eof,
			]
		);
	}

	#[test]
	fn test_block_comment()
	{
		let kinds = lex_kinds("/* block comment */ 42");
		assert_eq!(
			kinds,
			vec![
				// TokenKind::BlockComment(" block comment ".to_string()),
				TokenKind::IntLiteral(42),
				TokenKind::Eof,
			]
		);
	}

	#[test]
	fn test_block_doc_comment()
	{
		let kinds = lex_kinds("/** doc block comment */ 42");
		assert_eq!(
			kinds,
			vec![
				TokenKind::DocsComment(" doc block comment ".to_string()),
				TokenKind::IntLiteral(42),
				TokenKind::Eof,
			]
		);
	}

	#[test]
	fn test_multiline_block_comment()
	{
		let kinds = lex_kinds("/* line1\nline2\nline3 */ 42");
		assert_eq!(
			kinds,
			vec![
				// TokenKind::BlockComment(" line1\nline2\nline3 ".to_string()),
				TokenKind::IntLiteral(42),
				TokenKind::Eof,
			]
		);
	}

	// ===== Special Token Tests =====

	#[test]
	fn test_macros()
	{
		let kinds = lex_kinds("$foo $bar_baz $MyMacro");
		assert_eq!(
			kinds,
			vec![
				TokenKind::Macro("foo".to_string()),
				TokenKind::Macro("bar_baz".to_string()),
				TokenKind::Macro("MyMacro".to_string()),
				TokenKind::Eof,
			]
		);
	}

	#[test]
	fn test_directives()
	{
		let kinds = lex_kinds("@use @import @custom");
		assert_eq!(
			kinds,
			vec![
				TokenKind::Directive(Directive::Use),
				TokenKind::Directive(Directive::Import),
				TokenKind::Directive(Directive::Custom("custom".to_string())),
				TokenKind::Eof,
			]
		);
	}

	// ===== Error Switchs =====

	#[test]
	fn test_unterminated_string()
	{
		let kinds = lex_kinds(r#""unterminated"#);
		assert_eq!(kinds, vec![TokenKind::Invalid]);
	}

	#[test]
	fn test_unterminated_char()
	{
		let kinds = lex_kinds("'a");
		assert_eq!(kinds, vec![TokenKind::Label("a".to_string()), TokenKind::Eof]); // the paser should determine if a label can happen
	}

	#[test]
	fn test_invalid_char()
	{
		let kinds = lex_kinds("''");
		assert_eq!(kinds, vec![TokenKind::Invalid]);
	}

	#[test]
	fn test_invalid_characters()
	{
		let kinds = lex_kinds("@ $ `");
		assert_eq!(
			kinds,
			vec![
				TokenKind::Directive(Directive::Custom("".to_string())),
				TokenKind::Macro("".to_string()),
				TokenKind::Invalid,
			]
		);
	}

	// ===== Position Tracking Tests =====

	#[test]
	fn test_single_line_positions()
	{
		let tokens = lex_tokens("x + 42");

		assert_eq!(tokens[0].span.start_line, 1);
		assert_eq!(tokens[0].span.start_col, 1);
		assert_eq!(tokens[0].span.end_col, 2);

		assert_eq!(tokens[1].span.start_line, 1);
		assert_eq!(tokens[1].span.start_col, 3);
		assert_eq!(tokens[1].span.end_col, 4);

		assert_eq!(tokens[2].span.start_line, 1);
		assert_eq!(tokens[2].span.start_col, 5);
		assert_eq!(tokens[2].span.end_col, 7);
	}

	#[test]
	fn test_multiline_positions()
	{
		let tokens = lex_tokens("x\n+\n42");

		assert_eq!(tokens[0].span.start_line, 1);
		assert_eq!(tokens[1].span.start_line, 2);
		assert_eq!(tokens[2].span.start_line, 3);
	}

	#[test]
	fn test_byte_offsets()
	{
		let source = "x + 42";
		let tokens = lex_tokens(source);

		assert_eq!(&source[tokens[0].span.start..tokens[0].span.end], "x");
		assert_eq!(&source[tokens[1].span.start..tokens[1].span.end], "+");
		assert_eq!(&source[tokens[2].span.start..tokens[2].span.end], "42");
	}

	// ===== Integration Tests =====

	#[test]
	fn test_function_declaration()
	{
		let kinds = lex_kinds("fn add(x: i32, y: i32) -> i32 { x + y }");
		assert_eq!(
			kinds,
			vec![
				TokenKind::FuncDef,
				TokenKind::Identifier("add".to_string()),
				TokenKind::LeftParen,
				TokenKind::Identifier("x".to_string()),
				TokenKind::Colon,
				TokenKind::Identifier("i32".to_string()),
				TokenKind::Comma,
				TokenKind::Identifier("y".to_string()),
				TokenKind::Colon,
				TokenKind::Identifier("i32".to_string()),
				TokenKind::RightParen,
				TokenKind::Arrow,
				TokenKind::Identifier("i32".to_string()),
				TokenKind::LeftBrace,
				TokenKind::Identifier("x".to_string()),
				TokenKind::Plus,
				TokenKind::Identifier("y".to_string()),
				TokenKind::RightBrace,
				TokenKind::Eof,
			]
		);
	}

	#[test]
	fn test_struct_definition()
	{
		let kinds = lex_kinds("struct Point { x: i32, y: i32 }");
		assert_eq!(
			kinds,
			vec![
				TokenKind::Struct,
				TokenKind::Identifier("Point".to_string()),
				TokenKind::LeftBrace,
				TokenKind::Identifier("x".to_string()),
				TokenKind::Colon,
				TokenKind::Identifier("i32".to_string()),
				TokenKind::Comma,
				TokenKind::Identifier("y".to_string()),
				TokenKind::Colon,
				TokenKind::Identifier("i32".to_string()),
				TokenKind::RightBrace,
				TokenKind::Eof,
			]
		);
	}

	#[test]
	fn test_if_statement()
	{
		let kinds = lex_kinds("if x > 0 { x } else { -x }");
		assert_eq!(
			kinds,
			vec![
				TokenKind::If,
				TokenKind::Identifier("x".to_string()),
				TokenKind::GreaterThan,
				TokenKind::IntLiteral(0),
				TokenKind::LeftBrace,
				TokenKind::Identifier("x".to_string()),
				TokenKind::RightBrace,
				TokenKind::Else,
				TokenKind::LeftBrace,
				TokenKind::Minus,
				TokenKind::Identifier("x".to_string()),
				TokenKind::RightBrace,
				TokenKind::Eof,
			]
		);
	}

	#[test]
	fn test_for_loop()
	{
		let kinds = lex_kinds("for i in 0..10 { }");
		assert_eq!(
			kinds,
			vec![
				TokenKind::For,
				TokenKind::Identifier("i".to_string()),
				TokenKind::In,
				TokenKind::IntLiteral(0),
				TokenKind::DotDot,
				TokenKind::IntLiteral(10),
				TokenKind::LeftBrace,
				TokenKind::RightBrace,
				TokenKind::Eof,
			]
		);
	}

	#[test]
	fn test_switch_expression()
	{
		let kinds = lex_kinds("switch x { 0 => true, _ => false }");
		assert_eq!(
			kinds,
			vec![
				TokenKind::Switch,
				TokenKind::Identifier("x".to_string()),
				TokenKind::LeftBrace,
				TokenKind::IntLiteral(0),
				TokenKind::FatArrow,
				TokenKind::True,
				TokenKind::Comma,
				TokenKind::Underscore,
				TokenKind::FatArrow,
				TokenKind::False,
				TokenKind::RightBrace,
				TokenKind::Eof,
			]
		);
	}

	#[test]
	fn test_complex_expression()
	{
		let kinds = lex_kinds("(x + y) * z / (a - b)");
		assert_eq!(
			kinds,
			vec![
				TokenKind::LeftParen,
				TokenKind::Identifier("x".to_string()),
				TokenKind::Plus,
				TokenKind::Identifier("y".to_string()),
				TokenKind::RightParen,
				TokenKind::Star,
				TokenKind::Identifier("z".to_string()),
				TokenKind::Slash,
				TokenKind::LeftParen,
				TokenKind::Identifier("a".to_string()),
				TokenKind::Minus,
				TokenKind::Identifier("b".to_string()),
				TokenKind::RightParen,
				TokenKind::Eof,
			]
		);
	}

	#[test]
	fn test_memory_management_keywords()
	{
		let kinds = lex_kinds("delete");
		assert_eq!(kinds, vec![TokenKind::Delete, TokenKind::Eof,]);
	}
}
