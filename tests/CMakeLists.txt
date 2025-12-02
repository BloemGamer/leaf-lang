# -------------------------------------------------------
# Hash Tests
# -------------------------------------------------------
add_executable(test_hash test_hash.c
	${CMAKE_SOURCE_DIR}/src/utils/hash.c
)
target_include_directories(test_hash PRIVATE
	${CMAKE_SOURCE_DIR}/include
)
add_test(NAME HashTableTests COMMAND test_hash)

# -------------------------------------------------------
# Lexer Tests
# -------------------------------------------------------
add_executable(test_lexer test_lexer.c
	${CMAKE_SOURCE_DIR}/src/lexer.c
	${CMAKE_SOURCE_DIR}/src/tokens.c
	${CMAKE_SOURCE_DIR}/src/utils/hash.c
	# Add any other dependencies that lexer.c needs
)
target_include_directories(test_lexer PRIVATE
	${CMAKE_SOURCE_DIR}/include
)

# Register with CTest
add_test(NAME LexerTests COMMAND test_lexer)

# Set test properties
set_tests_properties(LexerTests PROPERTIES
	TIMEOUT 30
	PASS_REGULAR_EXPRESSION "Tests failed: 0"
)

# Optional: Add compiler warnings for tests
if(CMAKE_C_COMPILER_ID MATCHES "GNU|Clang")
	target_compile_options(test_hash PRIVATE
		-Wall -Wextra -Wpedantic
	)
	target_compile_options(test_lexer PRIVATE
		-Wall -Wextra -Wpedantic
	)
endif()
