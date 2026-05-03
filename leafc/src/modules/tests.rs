#[cfg(test)]
mod module_tests
{
	use crate::lexer::Lexer;
	use crate::lexer::expander::ExpandedLexer;
	use crate::modules::*;
	use crate::parser::*;
	use crate::source_map::SourceMap;
	use crate::{CompileError, Config};
	use std::path::PathBuf;

	fn parse_ast(input: &str) -> crate::parser::AST
	{
		let config = Config::default();
		let mut source_map = SourceMap::default();
		let lexer = Lexer::new_add_to_source_map(&config, input, "test", &mut source_map);
		let parser = Parser::from(ExpandedLexer::new(lexer));
		return parser.try_into().expect("failed to parse AST");
	}

	fn declaring_file() -> PathBuf
	{
		return PathBuf::from("/project/src/main.leaf");
	}

	// ── resolve_file (via collect_pending) ────────────────────────────────────

	#[test]
	fn test_external_module_resolves_to_sibling_file()
	{
		let ast = parse_ast("module foo;");
		let pending = collect_pending(&ast, &declaring_file(), &["root".to_string()]);
		assert_eq!(pending.len(), 1);
		assert_eq!(pending[0].logical_path, vec!["root".to_string(), "foo".to_string()]);
		assert_eq!(pending[0].file_path, PathBuf::from("/project/src/foo.leaf"));
	}

	#[test]
	fn test_external_module_nested_logical_path()
	{
		let ast = parse_ast("module bar;");
		let current = vec!["std".to_string(), "io".to_string()];
		let pending = collect_pending(&ast, &declaring_file(), &current);
		assert_eq!(
			pending[0].logical_path,
			vec!["std".to_string(), "io".to_string(), "bar".to_string()]
		);
	}

	#[test]
	fn test_external_qualified_module_name()
	{
		// module std::vec; should create a two-segment path
		let ast = parse_ast("module std::vec;");
		let pending = collect_pending(&ast, &declaring_file(), &[]);
		assert_eq!(pending.len(), 1);
		assert_eq!(pending[0].logical_path, vec!["std".to_string(), "vec".to_string()]);
		// file path: /project/src/std/vec.leaf
		assert_eq!(pending[0].file_path, PathBuf::from("/project/src/std/vec.leaf"));
	}

	#[test]
	fn test_no_modules_gives_empty_pending()
	{
		let ast = parse_ast("fn foo() {}");
		let pending = collect_pending(&ast, &declaring_file(), &[]);
		assert!(pending.is_empty());
	}

	#[test]
	fn test_multiple_external_modules()
	{
		let ast = parse_ast("module a; module b; module c;");
		let pending = collect_pending(&ast, &declaring_file(), &[]);
		assert_eq!(pending.len(), 3);
		let names: Vec<&str> = pending
			.iter()
			.map(|p| return p.logical_path.last().unwrap().as_str())
			.collect();
		assert_eq!(names, vec!["a", "b", "c"]);
	}

	// ── inline modules ────────────────────────────────────────────────────────

	#[test]
	fn test_inline_module_does_not_produce_pending()
	{
		// An inline module with no external sub-modules should yield nothing
		let ast = parse_ast("module utils { fn helper() {} }");
		let pending = collect_pending(&ast, &declaring_file(), &[]);
		assert!(pending.is_empty());
	}

	#[test]
	fn test_inline_module_with_external_child()
	{
		let ast = parse_ast("module outer { module inner; }");
		let pending = collect_pending(&ast, &declaring_file(), &[]);
		assert_eq!(pending.len(), 1);
		assert_eq!(pending[0].logical_path, vec!["outer".to_string(), "inner".to_string()]);
		// File path should NOT include "outer" as a directory segment because
		// file_path_segments is reset at the inline boundary per collect_from_block
		assert_eq!(pending[0].file_path, PathBuf::from("/project/src/inner.leaf"));
	}

	#[test]
	fn test_nested_inline_modules_with_external_leaf()
	{
		let ast = parse_ast("module a { module b { module c; } }");
		let pending = collect_pending(&ast, &declaring_file(), &[]);
		assert_eq!(pending.len(), 1);
		assert_eq!(
			pending[0].logical_path,
			vec!["a".to_string(), "b".to_string(), "c".to_string()]
		);
	}

	#[test]
	fn test_mixed_inline_and_external_siblings()
	{
		let ast = parse_ast("module inline_mod { fn foo() {} } module external_mod;");
		let pending = collect_pending(&ast, &declaring_file(), &[]);
		assert_eq!(pending.len(), 1);
		assert_eq!(pending[0].logical_path, vec!["external_mod".to_string()]);
	}

	// ── ModuleError Display ───────────────────────────────────────────────────

	#[test]
	fn test_display_file_not_found()
	{
		let e = ModuleError {
			logical_path: vec!["foo".to_string(), "bar".to_string()],
			span: Span::default(),
			kind: ModuleErrorKind::FileNotFound(PathBuf::from("/a/b/bar.leaf")),
			context: Vec::new(),
		};
		let s = e.to_string();
		assert!(s.contains("foo::bar"), "should contain logical path");
		assert!(s.contains("/a/b/bar.leaf"), "should contain file path");
		assert!(s.contains("file not found"));
	}

	#[test]
	fn test_display_io_error()
	{
		let e = ModuleError {
			logical_path: vec!["my_mod".to_string()],
			span: Span::default(),
			kind: ModuleErrorKind::IoError("permission denied".to_string()),
			context: Vec::new(),
		};
		let s = e.to_string();
		assert!(s.contains("my_mod"));
		assert!(s.contains("permission denied"));
	}

	#[test]
	fn test_display_cycle()
	{
		let e = ModuleError {
			logical_path: vec!["a".to_string()],
			span: Span::default(),
			kind: ModuleErrorKind::Cycle(vec![
				vec!["a".to_string(), "b".to_string()],
				vec!["c".to_string()],
				vec!["a".to_string(), "b".to_string()],
			]),
			context: Vec::new(),
		};
		let s = e.to_string();
		assert!(s.contains("cycle"), "should mention cycle");
		assert!(s.contains("a::b"));
		assert!(s.contains("->"), "should show chain with arrows");
	}

	#[test]
	fn test_compile_error_from_module_error()
	{
		let e = ModuleError {
			logical_path: vec!["x".to_string()],
			span: Span::default(),
			kind: ModuleErrorKind::IoError("oops".to_string()),
			context: Vec::new(),
		};
		let ce: CompileError = e.into();
		assert!(matches!(ce, CompileError::Module(_)));
	}

	// ── file_path resolves relative to declaring_file ─────────────────────────

	#[test]
	fn test_file_path_relative_to_declaring_file()
	{
		let declaring = PathBuf::from("/home/user/project/src/lib.leaf");
		let ast = parse_ast("module utils;");
		let pending = collect_pending(&ast, &declaring, &[]);
		assert_eq!(pending[0].file_path, PathBuf::from("/home/user/project/src/utils.leaf"));
	}

	#[test]
	fn test_file_path_no_parent_falls_back_to_dot()
	{
		let declaring = PathBuf::from("main.leaf"); // no parent
		let ast = parse_ast("module helper;");
		let pending = collect_pending(&ast, &declaring, &[]);
		assert_eq!(pending[0].file_path, PathBuf::from("helper.leaf"));
	}

	// ── declared_at metadata ─────────────────────────────────────────────────

	#[test]
	fn test_pending_module_records_source_index()
	{
		let ast = parse_ast("module net;");
		let pending = collect_pending(&ast, &declaring_file(), &[]);
		// The source index should match the AST's source index
		assert_eq!(pending[0].declared_at_span.source_index, ast.source_index);
	}
}
