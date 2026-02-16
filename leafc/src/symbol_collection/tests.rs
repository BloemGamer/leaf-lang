#[cfg(test)]
#[allow(clippy::module_inception)]
mod tests
{
	use crate::{
		Config,
		desugar::Desugarer,
		lexer::Lexer,
		parser::Parser,
		source_map::SourceIndex,
		symbol_collection::{
			Mutability, ScopeKind, Symbol, SymbolCollectionError, SymbolId, SymbolKind, SymbolTable, Visibility,
			collect_symbols,
		},
	};

	fn parse_and_collect(source: &str) -> Result<SymbolTable, SymbolCollectionError>
	{
		let config = Config::default();
		let source_index = SourceIndex::new(0);
		let lexer = Lexer::new(&config, source, source_index);
		let mut parser = Parser::from(lexer);
		let program = parser.parse_program().unwrap();

		let mut desugarer = Desugarer::new(SourceIndex::new(0));
		let desugared = desugarer.desugar_program(program).unwrap();
		println!("{}", desugared);

		return collect_symbols(&desugared, source_index);
	}

	fn find_symbol_by_name<'a>(table: &'a SymbolTable, name: &str) -> Option<(SymbolId, &'a Symbol)>
	{
		for (idx, symbol) in table.symbols.iter().enumerate() {
			if symbol.name == name {
				return Some((SymbolId(idx), symbol));
			}
		}
		return None;
	}

	#[test]
	fn test_simple_function()
	{
		let source = r#"
			fn main() {
				return;
			}
		"#;

		let table = parse_and_collect(source).unwrap();

		let (_, main_symbol) = find_symbol_by_name(&table, "main").expect("main function not found");
		assert_eq!(main_symbol.name, "main");
		assert!(matches!(main_symbol.kind, SymbolKind::Function { comp_const: false }));
		assert_eq!(main_symbol.visibility, Visibility::Private);
	}

	#[test]
	fn test_public_function()
	{
		let source = r#"
			pub fn test() {
				return;
			}
		"#;

		let table = parse_and_collect(source).unwrap();
		let (_, test_symbol) = find_symbol_by_name(&table, "test").expect("test function not found");
		assert_eq!(test_symbol.visibility, Visibility::Public);
	}

	#[test]
	fn test_const_function()
	{
		let source = r#"
			const fn compute() {
				return;
			}
		"#;

		let table = parse_and_collect(source).unwrap();
		let (_, compute_symbol) = find_symbol_by_name(&table, "compute").expect("compute function not found");
		assert!(matches!(compute_symbol.kind, SymbolKind::Function { comp_const: true }));
	}

	#[test]
	fn test_function_parameters()
	{
		let source = r#"
			fn add(x: i32, y: i32) {
				return;
			}
		"#;

		let table = parse_and_collect(source).unwrap();

		let (_, add_symbol) = find_symbol_by_name(&table, "add").expect("add function not found");
		let add_scope = add_symbol.scope;

		// Check that parameters are in the function scope
		let (_, x_symbol) = find_symbol_by_name(&table, "x").expect("x parameter not found");
		let (_, y_symbol) = find_symbol_by_name(&table, "y").expect("y parameter not found");

		assert_ne!(x_symbol.scope, add_scope);
		assert_ne!(y_symbol.scope, add_scope);
		assert_eq!(x_symbol.scope, y_symbol.scope);
		assert!(matches!(
			x_symbol.kind,
			SymbolKind::Variable {
				mutability: Mutability::Immutable
			}
		));
		assert!(matches!(
			y_symbol.kind,
			SymbolKind::Variable {
				mutability: Mutability::Immutable
			}
		));
	}

	#[test]
	fn test_mutable_parameter()
	{
		let source = r#"
			fn modify(mut x: i32) {
				return;
			}
		"#;

		let table = parse_and_collect(source).unwrap();
		println!("{:#?}", table);
		let (_, x_symbol) = find_symbol_by_name(&table, "x").expect("x parameter not found");
		println!("{:?}", x_symbol);
		assert!(matches!(
			x_symbol.kind,
			SymbolKind::Variable {
				mutability: Mutability::Mutable
			}
		));
	}

	#[test]
	fn test_generic_parameters()
	{
		let source = r#"
			fn generic<T, U>() {
				return;
			}
		"#;

		let table = parse_and_collect(source).unwrap();

		let (_, t_symbol) = find_symbol_by_name(&table, "T").expect("T generic not found");
		let (_, u_symbol) = find_symbol_by_name(&table, "U").expect("U generic not found");

		assert!(matches!(t_symbol.kind, SymbolKind::GenericParam));
		assert!(matches!(u_symbol.kind, SymbolKind::GenericParam));
	}

	#[test]
	fn test_heap_generics()
	{
		let source = r#"
			fn!<IO, Alloc> heap_func() {
				return;
			}
		"#;

		let table = parse_and_collect(source).unwrap();

		// IO and Alloc should be in the symbol table as generic params
		let (_, io_symbol) = find_symbol_by_name(&table, "IO").expect("IO generic not found");
		let (_, alloc_symbol) = find_symbol_by_name(&table, "Alloc").expect("Alloc generic not found");

		assert!(matches!(io_symbol.kind, SymbolKind::GenericParam));
		assert!(matches!(alloc_symbol.kind, SymbolKind::GenericParam));
	}

	#[test]
	fn test_variable_declaration()
	{
		let source = r#"
			fn main() {
				var x: i32 = 5;
				return;
			}
		"#;

		let table = parse_and_collect(source).unwrap();
		let (_, x_symbol) = find_symbol_by_name(&table, "x").expect("x variable not found");
		assert!(matches!(
			x_symbol.kind,
			SymbolKind::Variable {
				mutability: Mutability::Immutable
			}
		));
	}

	#[test]
	fn test_mutable_variable()
	{
		let source = r#"
			fn main() {
				var mut x: i32 = 5;
				return;
			}
		"#;

		let table = parse_and_collect(source).unwrap();
		let (_, x_symbol) = find_symbol_by_name(&table, "x").expect("x variable not found");
		assert!(matches!(
			x_symbol.kind,
			SymbolKind::Variable {
				mutability: Mutability::Mutable
			}
		));
	}

	#[test]
	fn test_const_variable()
	{
		let source = r#"
			const PI: f64 = 3.14159;
		"#;

		let table = parse_and_collect(source).unwrap();
		let (_, pi_symbol) = find_symbol_by_name(&table, "PI").expect("PI constant not found");
		assert!(matches!(
			pi_symbol.kind,
			SymbolKind::Variable {
				mutability: Mutability::Const
			}
		));
	}

	#[test]
	fn test_struct_declaration()
	{
		let source = r#"
			struct Point {
				x: i32,
				y: i32,
			}
		"#;

		let table = parse_and_collect(source).unwrap();

		let (_, point_symbol) = find_symbol_by_name(&table, "Point").expect("Point struct not found");
		assert!(matches!(point_symbol.kind, SymbolKind::Struct));

		let (_, x_symbol) = find_symbol_by_name(&table, "x").expect("x field not found");
		let (_, y_symbol) = find_symbol_by_name(&table, "y").expect("y field not found");

		assert!(matches!(x_symbol.kind, SymbolKind::Field));
		assert!(matches!(y_symbol.kind, SymbolKind::Field));
	}

	#[test]
	fn test_public_struct_fields()
	{
		let source = r#"
			pub struct Point {
				pub x: i32,
				y: i32,
			}
		"#;

		let table = parse_and_collect(source).unwrap();

		let (_, point_symbol) = find_symbol_by_name(&table, "Point").expect("Point struct not found");
		assert_eq!(point_symbol.visibility, Visibility::Public);

		let (_, x_symbol) = find_symbol_by_name(&table, "x").expect("x field not found");
		let (_, y_symbol) = find_symbol_by_name(&table, "y").expect("y field not found");

		assert_eq!(x_symbol.visibility, Visibility::Public);
		assert_eq!(y_symbol.visibility, Visibility::Private);
	}

	#[test]
	fn test_union_declaration()
	{
		let source = r#"
			union Data {
				i: i32,
				f: f32,
			}
		"#;

		let table = parse_and_collect(source).unwrap();

		let (_, data_symbol) = find_symbol_by_name(&table, "Data").expect("Data union not found");
		assert!(matches!(data_symbol.kind, SymbolKind::Union));

		let (_, i_symbol) = find_symbol_by_name(&table, "i").expect("i field not found");
		let (_, f_symbol) = find_symbol_by_name(&table, "f").expect("f field not found");

		assert!(matches!(i_symbol.kind, SymbolKind::Field));
		assert!(matches!(f_symbol.kind, SymbolKind::Field));
	}

	#[test]
	fn test_enum_declaration()
	{
		let source = r#"
			enum Color {
				Red,
				Green,
				Blue,
			}
		"#;

		let table = parse_and_collect(source).unwrap();

		let (_, color_symbol) = find_symbol_by_name(&table, "Color").expect("Color enum not found");
		assert!(matches!(color_symbol.kind, SymbolKind::Enum));

		let (_, red_symbol) = find_symbol_by_name(&table, "Red").expect("Red variant not found");
		let (_, green_symbol) = find_symbol_by_name(&table, "Green").expect("Green variant not found");
		let (_, blue_symbol) = find_symbol_by_name(&table, "Blue").expect("Blue variant not found");

		assert!(matches!(red_symbol.kind, SymbolKind::EnumVariant));
		assert!(matches!(green_symbol.kind, SymbolKind::EnumVariant));
		assert!(matches!(blue_symbol.kind, SymbolKind::EnumVariant));
	}

	#[test]
	fn test_variant_declaration()
	{
		let source = r#"
			variant Option {
				Some(i32),
				None,
			}
		"#;

		let table = parse_and_collect(source).unwrap();

		let (_, option_symbol) = find_symbol_by_name(&table, "Option").expect("Option variant not found");
		assert!(matches!(option_symbol.kind, SymbolKind::Variant));

		let (_, some_symbol) = find_symbol_by_name(&table, "Some").expect("Some member not found");
		let (_, none_symbol) = find_symbol_by_name(&table, "None").expect("None member not found");

		assert!(matches!(some_symbol.kind, SymbolKind::VariantMember));
		assert!(matches!(none_symbol.kind, SymbolKind::VariantMember));
	}

	#[test]
	fn test_type_alias()
	{
		let source = r#"
			type Integer = i32;
		"#;

		let table = parse_and_collect(source).unwrap();
		let (_, int_symbol) = find_symbol_by_name(&table, "Integer").expect("Integer alias not found");
		assert!(matches!(int_symbol.kind, SymbolKind::TypeAlias));
	}

	#[test]
	fn test_trait_declaration()
	{
		let source = r#"
			trait Display {
				fn show();
			}
		"#;

		let table = parse_and_collect(source).unwrap();

		let (_, display_symbol) = find_symbol_by_name(&table, "Display").expect("Display trait not found");
		assert!(matches!(display_symbol.kind, SymbolKind::Trait));

		let (_, show_symbol) = find_symbol_by_name(&table, "show").expect("show method not found");
		assert!(matches!(show_symbol.kind, SymbolKind::Function { comp_const: false }));
	}

	#[test]
	fn test_trait_with_generics()
	{
		let source = r#"
			trait Container<T> {
				fn get() -> T;
			}
		"#;

		let table = parse_and_collect(source).unwrap();

		let (_, container_symbol) = find_symbol_by_name(&table, "Container").expect("Container trait not found");
		assert!(matches!(container_symbol.kind, SymbolKind::Trait));

		let (_, t_symbol) = find_symbol_by_name(&table, "T").expect("T generic not found");
		assert!(matches!(t_symbol.kind, SymbolKind::GenericParam));
	}

	#[test]
	fn test_module_declaration()
	{
		let source = r#"
			module utils {
				fn helper() {
					return;
				}
			}
		"#;

		let table = parse_and_collect(source).unwrap();

		let (_, utils_symbol) = find_symbol_by_name(&table, "utils").expect("utils module not found");
		assert!(matches!(utils_symbol.kind, SymbolKind::Module));

		let (_, helper_symbol) = find_symbol_by_name(&table, "helper").expect("helper function not found");
		assert!(matches!(helper_symbol.kind, SymbolKind::Function { comp_const: false }));
	}

	#[test]
	fn test_impl_block()
	{
		let source = r#"
			struct Point {
				x: i32,
				y: i32,
			}

			impl Point {
				fn new() -> Point {
					return Point { x -> 0, y -> 0 };
				}
			}
		"#;

		let table = parse_and_collect(source).unwrap();
		let (_, new_symbol) = find_symbol_by_name(&table, "new").expect("new method not found");
		assert!(matches!(new_symbol.kind, SymbolKind::Function { comp_const: false }));
	}

	#[test]
	fn test_impl_with_generics()
	{
		let source = r#"
			struct Container<T> {
				value: T,
			}

			impl<T> Container<T> {
				fn get() -> T {
					return value;
				}
			}
		"#;

		let table = parse_and_collect(source).unwrap();
		let (_, t_symbol) = find_symbol_by_name(&table, "T").expect("T generic not found");
		assert!(matches!(t_symbol.kind, SymbolKind::GenericParam));
	}

	#[test]
	fn test_nested_blocks()
	{
		let source = r#"
			fn main() {
				var x: i32 = 1;
				{
					var y: i32 = 2;
					{
						var z: i32 = 3;
					}
				}
				return;
			}
		"#;

		let table = parse_and_collect(source).unwrap();

		let (_, x_symbol) = find_symbol_by_name(&table, "x").expect("x variable not found");
		let (_, y_symbol) = find_symbol_by_name(&table, "y").expect("y variable not found");
		let (_, z_symbol) = find_symbol_by_name(&table, "z").expect("z variable not found");

		// All should be in different scopes
		assert_ne!(x_symbol.scope, y_symbol.scope);
		assert_ne!(y_symbol.scope, z_symbol.scope);
		assert_ne!(x_symbol.scope, z_symbol.scope);
	}

	#[test]
	fn test_loop_with_label()
	{
		let source = r#"
			fn main() {
				loop {
					break;
				}
				return;
			}
		"#;

		let table = parse_and_collect(source).unwrap();

		// Check that the generated loop label exists
		let label_exists = table.symbols.iter().any(|s| matches!(s.kind, SymbolKind::Label));
		assert!(label_exists, "Loop label not found");
	}

	#[test]
	fn test_if_statement_scopes()
	{
		let source = r#"
			fn main() {
				if true {
					var x: i32 = 1;
				} else {
					var y: i32 = 2;
				}
				return;
			}
		"#;

		let table = parse_and_collect(source).unwrap();

		let (_, x_symbol) = find_symbol_by_name(&table, "x").expect("x variable not found");
		let (_, y_symbol) = find_symbol_by_name(&table, "y").expect("y variable not found");

		// x and y should be in different scopes
		assert_ne!(x_symbol.scope, y_symbol.scope);

		let x_scope = table.scope(x_symbol.scope);
		let y_scope = table.scope(y_symbol.scope);

		assert!(matches!(x_scope.kind, ScopeKind::IfThen));
		assert!(matches!(y_scope.kind, ScopeKind::ElseBlock));
	}

	#[test]
	fn test_switch_arm_scopes()
	{
		let source = r#"
			fn main() {
				var value: i32 = 0;
				switch value {
					1 => {
						var x: i32 = 10;
					},
					2 => {
						var y: i32 = 20;
					},
					_ => {},
				}
				return;
			}
		"#;

		let table = parse_and_collect(source).unwrap();

		let (_, x_symbol) = find_symbol_by_name(&table, "x").expect("x variable not found");
		let (_, y_symbol) = find_symbol_by_name(&table, "y").expect("y variable not found");

		assert_ne!(x_symbol.scope, y_symbol.scope);

		let x_scope = table.scope(x_symbol.scope);
		let y_scope = table.scope(y_symbol.scope);

		assert!(matches!(x_scope.kind, ScopeKind::SwitchArm));
		assert!(matches!(y_scope.kind, ScopeKind::SwitchArm));
	}

	#[test]
	fn test_duplicate_definition_error()
	{
		let source = r#"
			fn test() {
				return;
			}

			fn test() {
				return;
			}
		"#;

		let result = parse_and_collect(source);
		assert!(result.is_err());

		if let Err(err) = result {
			assert!(matches!(
				err.kind,
				crate::symbol_collection::SymbolCollectionErrorKind::DuplicateDefinition { .. }
			));
		}
	}

	#[test]
	fn test_duplicate_parameter_error()
	{
		let source = r#"
			fn test(x: i32, x: i32) {
				return;
			}
		"#;

		let result = parse_and_collect(source);
		assert!(result.is_err());
	}

	#[test]
	fn test_duplicate_field_error()
	{
		let source = r#"
			struct Point {
				x: i32,
				x: i32,
			}
		"#;

		let result = parse_and_collect(source);
		assert!(result.is_err());
	}

	#[test]
	fn test_labels_dont_conflict_with_variables()
	{
		let source = r#"
			fn main() {
				var x: i32 = 0;
				loop {
					break;
				}
				return;
			}
		"#;

		// This should succeed - labels are in a separate namespace
		let table = parse_and_collect(source).unwrap();
		let (_, x_symbol) = find_symbol_by_name(&table, "x").expect("x variable not found");
		assert!(matches!(
			x_symbol.kind,
			SymbolKind::Variable {
				mutability: Mutability::Immutable
			}
		));
	}

	#[test]
	fn test_scope_hierarchy()
	{
		let source = r#"
			fn outer() {
				var x: i32 = 1;
				{
					var y: i32 = 2;
				}
				return;
			}
		"#;

		let table = parse_and_collect(source).unwrap();

		let (_, x_symbol) = find_symbol_by_name(&table, "x").expect("x variable not found");
		let (_, y_symbol) = find_symbol_by_name(&table, "y").expect("y variable not found");

		let x_scope = table.scope(x_symbol.scope);
		let y_scope = table.scope(y_symbol.scope);

		// y's scope should be a child of x's scope
		assert_eq!(y_scope.parent, Some(x_symbol.scope));
		assert!(x_scope.children.contains(&y_symbol.scope));
	}

	#[test]
	fn test_root_scope()
	{
		let source = r#"
			fn main() {
				return;
			}
		"#;

		let table = parse_and_collect(source).unwrap();

		let root_scope = table.scope(table.root);
		assert!(matches!(root_scope.kind, ScopeKind::ModuleInline));
		assert!(root_scope.parent.is_none());
	}

	#[test]
	fn test_multiple_modules()
	{
		let source = r#"
			module math {
				fn add() {
					return;
				}
			}

			module string {
				fn concat() {
					return;
				}
			}
		"#;

		let table = parse_and_collect(source).unwrap();

		let (_, math_symbol) = find_symbol_by_name(&table, "math").expect("math module not found");
		let (_, string_symbol) = find_symbol_by_name(&table, "string").expect("string module not found");

		assert!(matches!(math_symbol.kind, SymbolKind::Module));
		assert!(matches!(string_symbol.kind, SymbolKind::Module));

		// Modules should be in the same scope, because they're both in the global scope
		assert_eq!(math_symbol.scope, string_symbol.scope);
	}

	#[test]
	fn test_visibility_propagation()
	{
		let source = r#"
			pub enum Color {
				Red,
				Green,
			}
		"#;

		let table = parse_and_collect(source).unwrap();

		let (_, color_symbol) = find_symbol_by_name(&table, "Color").expect("Color enum not found");
		let (_, red_symbol) = find_symbol_by_name(&table, "Red").expect("Red variant not found");

		assert_eq!(color_symbol.visibility, Visibility::Public);
		assert_eq!(red_symbol.visibility, Visibility::Public);
	}

	#[test]
	fn test_impl_trait()
	{
		let source = r#"
			trait Show {
				fn display();
			}

			struct Point {
				x: i32,
			}

			impl Show for Point {
				fn display() {
					return;
				}
			}
		"#;

		let table = parse_and_collect(source).unwrap();

		let (_, show_trait) = find_symbol_by_name(&table, "Show").expect("Show trait not found");
		let (_, point_struct) = find_symbol_by_name(&table, "Point").expect("Point struct not found");

		assert!(matches!(show_trait.kind, SymbolKind::Trait));
		assert!(matches!(point_struct.kind, SymbolKind::Struct));

		// Should have two display functions - one in trait, one in impl
		assert_eq!(table.symbols.iter().filter(|s| return s.name == "display").count(), 2);
	}

	#[test]
	fn test_complex_nesting()
	{
		let source = r#"
			module outer {
				pub struct Data {
					value: i32,
				}

				impl Data {
					fn process() {
						var temp: i32 = 0;
						{
							var inner: i32 = 1;
						}
						return;
					}
				}
			}
		"#;

		let table = parse_and_collect(source).unwrap();

		let (_, outer_mod) = find_symbol_by_name(&table, "outer").expect("outer module not found");
		let (_, data_struct) = find_symbol_by_name(&table, "Data").expect("Data struct not found");
		let (_, process_fn) = find_symbol_by_name(&table, "process").expect("process function not found");
		let (_, temp_var) = find_symbol_by_name(&table, "temp").expect("temp variable not found");
		let (_, inner_var) = find_symbol_by_name(&table, "inner").expect("inner variable not found");

		assert!(matches!(outer_mod.kind, SymbolKind::Module));
		assert!(matches!(data_struct.kind, SymbolKind::Struct));
		assert!(matches!(process_fn.kind, SymbolKind::Function { comp_const: false }));

		// Check scope relationships
		let inner_scope = table.scope(inner_var.scope);
		let temp_scope = table.scope(temp_var.scope);

		assert_eq!(inner_scope.parent, Some(temp_var.scope));
		assert!(temp_scope.children.contains(&inner_var.scope));
	}
}
