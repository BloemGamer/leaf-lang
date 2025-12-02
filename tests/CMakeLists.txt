# hash
	# Test executable
	add_executable(test_hash test_hash.c)

	target_include_directories(test_hash PRIVATE
		${CMAKE_SOURCE_DIR}/include
	)

	target_link_libraries(test_hash PRIVATE hash)

	# Register with CTest
	add_test(NAME HashTableTests COMMAND test_hash)

