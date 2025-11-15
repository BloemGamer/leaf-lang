# hash
	# Test executable
	add_executable(hash_test hash_test.c)

	target_include_directories(hash_test PRIVATE
		${CMAKE_SOURCE_DIR}/include
	)

	target_link_libraries(hash_test PRIVATE hash)

	# Register with CTest
	add_test(NAME HashTableTests COMMAND hash_test)

