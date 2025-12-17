# -------------------------------------------------------
# Hash Tests
# -------------------------------------------------------
add_executable(test_hash test_hash.c
	${CMAKE_SOURCE_DIR}/csrc/utils/hash.c
)
target_include_directories(test_hash PRIVATE
	${CMAKE_SOURCE_DIR}/include
)
add_test(NAME HashTableTests COMMAND test_hash)

# set_tests_properties(HahsTableTests PROPERTIES
# 	TIMEOUT 1000
# 	PASS_REGULAR_EXPRESSION "Tests failed: 0"
# )

# -------------------------------------------------------
# Lexer Tests
# -------------------------------------------------------
add_executable(test_lexer test_lexer.c
	${CMAKE_SOURCE_DIR}/csrc/lexer.c
	${CMAKE_SOURCE_DIR}/csrc/tokens.c
	${CMAKE_SOURCE_DIR}/csrc/utils/hash.c
	# Add any other dependencies that lexer.c needs
)
target_include_directories(test_lexer PRIVATE
	${CMAKE_SOURCE_DIR}/include
)

# Register with CTest
add_test(NAME LexerTests COMMAND test_lexer)

# Set test properties
set_tests_properties(LexerTests PROPERTIES
	TIMEOUT 1000
	PASS_REGULAR_EXPRESSION "Tests failed: 0"
)

# -------------------------------------------------------
# Parser Tests
# -------------------------------------------------------
add_executable(test_parser test_parser.c
	${CMAKE_SOURCE_DIR}/csrc/lexer.c
	${CMAKE_SOURCE_DIR}/csrc/tokens.c
	${CMAKE_SOURCE_DIR}/csrc/parser.c
	${CMAKE_SOURCE_DIR}/csrc/utils/hash.c
	# Add any other dependencies that parser.c needs
)

target_include_directories(test_parser PRIVATE
	${CMAKE_SOURCE_DIR}/include
)

# Register with CTest
add_test(NAME ParserTests COMMAND test_parser)

# Set test properties
set_tests_properties(ParserTests PROPERTIES
	TIMEOUT 1000
	PASS_REGULAR_EXPRESSION "Tests failed: 0"
)

# -------------------------------------------------------
# Code Gen Tests
# -------------------------------------------------------
add_executable(test_code_gen test_code_gen.c
	${CMAKE_SOURCE_DIR}/csrc/lexer.c
	${CMAKE_SOURCE_DIR}/csrc/tokens.c
	${CMAKE_SOURCE_DIR}/csrc/parser.c
	${CMAKE_SOURCE_DIR}/csrc/code_gen.c
	${CMAKE_SOURCE_DIR}/csrc/utils/hash.c
	# Add any other dependencies that code_gen.c needs
)

target_include_directories(test_code_gen PRIVATE
	${CMAKE_SOURCE_DIR}/include
)

# Register with CTest
add_test(NAME CodeGenTests COMMAND test_code_gen)

# Set test properties
set_tests_properties(CodeGenTests PROPERTIES
	TIMEOUT 1000
	PASS_REGULAR_EXPRESSION "Tests failed: 0"
)

# -------------------------------------------------------
# Default things
# -------------------------------------------------------

# Optional: Add compiler warnings for tests
if(CMAKE_C_COMPILER_ID MATCHES "GNU|Clang")
	target_compile_options(test_hash PRIVATE
		-Wall -Wextra -Wpedantic
	)
	target_compile_options(test_lexer PRIVATE
		-Wall -Wextra -Wpedantic
	)
endif()
