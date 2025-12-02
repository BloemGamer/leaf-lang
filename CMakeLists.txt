cmake_minimum_required(VERSION 3.10)
project(s-lang VERSION 0.1.0 LANGUAGES C)

set(CMAKE_C_STANDARD 23)
set(CMAKE_C_STANDARD_REQUIRED ON)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
set(CMAKE_C_EXTENSIONS OFF)

# set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_SOURCE_DIR})


# add_compile_options(-std=gnu2y)

# add_compile_options(-fsanitize=address)
# add_link_options(-fsanitize=address)

enable_testing()


# -------------------------------------------------------
# Main executable
# -------------------------------------------------------
add_executable(s-lang
	src/main.c
	src/tokens.c
	src/lexer.c
	src/parser.c
	src/code_gen.c

	src/utils/hash.c
)

target_include_directories(s-lang PRIVATE include)

# This was needed because of a weird build setup that I had, not needed anymore
# add_custom_command(
# 	TARGET s-lang POST_BUILD
# 	COMMAND sed -i "s#/run/host##g" ${CMAKE_BINARY_DIR}/compile_commands.json
# )

# -------------------------------------------------------
# Tests directory
# -------------------------------------------------------
add_subdirectory(tests)

if(CMAKE_C_COMPILER_ID MATCHES "GNU|Clang")
	target_compile_options(s-lang PRIVATE
		-Wall -Wextra -Wpedantic -Wconversion -Wsign-conversion
		-Wshadow -Wdouble-promotion -Wformat=2
	)
endif()
