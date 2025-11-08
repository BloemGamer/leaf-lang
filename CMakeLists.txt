cmake_minimum_required(VERSION 3.10)
project(s-lang VERSION 0.1.0 LANGUAGES C)

set(CMAKE_C_STANDARD 23)
set(CMAKE_C_STANDARD_REQUIRED ON)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
set(CMAKE_BUILD_TYPE Debug)



set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/..)

add_compile_options(-std=gnu2y)

# add_compile_options(-fsanitize=address)
# add_link_options(-fsanitize=address)



add_executable(s-lang
	src/main.c
	src/lexer.c
	src/parser.c
	src/tokens.c
)

target_include_directories(s-lang
	PRIVATE
		include
)

# needed of my setup, but probably not for the one of other people
add_custom_command(
	TARGET s-lang POST_BUILD
	COMMAND sed -i "s#/run/host##g" ${CMAKE_BINARY_DIR}/compile_commands.json
)
