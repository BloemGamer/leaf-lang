#include <assert.h>
#include <stdio.h>

#include "utils/hash.h"

void test_basic_operations()
{
	printf("Test 1: Basic insert and contains\n");
	HashStr hash = hash_str_new(8);

	assert(hash_str_push(&hash, "hello") == false); // First insert
	assert(hash_str_contains(&hash, "hello") == true);
	assert(hash.size == 1);

	assert(hash_str_push(&hash, "hello") == true); // Duplicate
	assert(hash.size == 1);						   // Size shouldn't change

	hash_str_free(&hash);
	printf("✓ Passed\n\n");
}

void test_collision_handling()
{
	printf("Test 2: Collision handling\n");
	HashStr hash = hash_str_new(4); // Small capacity forces collisions

	hash_str_push(&hash, "test1");
	hash_str_push(&hash, "test2");
	hash_str_push(&hash, "test3");

	assert(hash_str_contains(&hash, "test1"));
	assert(hash_str_contains(&hash, "test2"));
	assert(hash_str_contains(&hash, "test3"));
	assert(hash.size == 3);

	hash_str_free(&hash);
	printf("✓ Passed\n\n");
}

void test_resize_trigger()
{
	printf("Test 3: Resize trigger (CRITICAL BUG TEST)\n");
	HashStr hash = hash_str_new(4);

	// Insert until resize is triggered (size * 2 >= cap)
	// With cap=4, resize triggers when size >= 2
	hash_str_push(&hash, "item1"); // size=1
	hash_str_push(&hash, "item2"); // size=2, should trigger resize
	hash_str_push(&hash, "item3"); // size=3

	printf("After resize - cap: %lu, size: %lu\n", hash.cap, hash.size);

	// Verify all items are still accessible
	assert(hash_str_contains(&hash, "item1"));
	assert(hash_str_contains(&hash, "item2"));
	assert(hash_str_contains(&hash, "item3"));

	hash_str_free(&hash);
	printf("✓ Passed\n\n");
}

void test_remove_operations()
{
	printf("Test 4: Remove operations\n");
	HashStr hash = hash_str_new(8);

	hash_str_push(&hash, "remove_me");
	hash_str_push(&hash, "keep_me");

	assert(hash_str_remove(&hash, "remove_me") == true);
	assert(hash_str_contains(&hash, "remove_me") == false);
	assert(hash_str_contains(&hash, "keep_me") == true);
	assert(hash.size == 1);

	// Remove non-existent
	assert(hash_str_remove(&hash, "never_existed") == false);
	assert(hash.size == 1);

	hash_str_free(&hash);
	printf("✓ Passed\n\n");
}

void test_remove_from_chain()
{
	printf("Test 5: Remove from collision chain\n");
	HashStr hash = hash_str_new(2); // Force collisions

	hash_str_push(&hash, "a");
	hash_str_push(&hash, "b");
	hash_str_push(&hash, "c");

	// Remove middle of chain
	hash_str_remove(&hash, "b");

	assert(hash_str_contains(&hash, "a"));
	assert(hash_str_contains(&hash, "b") == false);
	assert(hash_str_contains(&hash, "c"));

	hash_str_free(&hash);
	printf("✓ Passed\n\n");
}

void test_clone()
{
	printf("Test 6: Clone operation\n");
	HashStr hash = hash_str_new(8);

	hash_str_push(&hash, "one");
	hash_str_push(&hash, "two");
	hash_str_push(&hash, "three");

	HashStr cloned = hash_str_clone(&hash);

	assert(cloned.size == hash.size);
	assert(cloned.cap == hash.cap);
	assert(hash_str_contains(&cloned, "one"));
	assert(hash_str_contains(&cloned, "two"));
	assert(hash_str_contains(&cloned, "three"));

	// Modify original, clone should be unaffected
	hash_str_push(&hash, "four");
	assert(hash_str_contains(&hash, "four"));
	assert(hash_str_contains(&cloned, "four") == false);

	hash_str_free(&hash);
	hash_str_free(&cloned);
	printf("✓ Passed\n\n");
}

void test_empty_string()
{
	printf("Test 7: Empty string handling\n");
	HashStr hash = hash_str_new(8);

	hash_str_push(&hash, "");
	assert(hash_str_contains(&hash, "") == true);
	assert(hash.size == 1);

	hash_str_remove(&hash, "");
	assert(hash_str_contains(&hash, "") == false);

	hash_str_free(&hash);
	printf("✓ Passed\n\n");
}

void test_large_scale()
{
	printf("Test 8: Large scale operations (stress test)\n");
	HashStr hash = hash_str_new(8);
	char buffer[100];

	// Insert 1000 items
	for (int i = 0; i < 1000; i++)
	{
		sprintf(buffer, "item_%d", i);
		hash_str_push(&hash, buffer);
	}

	assert(hash.size == 1000);
	printf("Inserted 1000 items, final capacity: %lu\n", hash.cap);

	// Verify all items exist
	for (int i = 0; i < 1000; i++)
	{
		sprintf(buffer, "item_%d", i);
		assert(hash_str_contains(&hash, buffer));
	}

	// Remove half
	for (int i = 0; i < 500; i++)
	{
		sprintf(buffer, "item_%d", i);
		hash_str_remove(&hash, buffer);
	}

	assert(hash.size == 500);

	hash_str_free(&hash);
	printf("✓ Passed\n\n");
}

void test_special_characters()
{
	printf("Test 9: Special characters\n");
	HashStr hash = hash_str_new(8);

	hash_str_push(&hash, "hello world");
	hash_str_push(&hash, "tab\there");
	hash_str_push(&hash, "newline\n");
	hash_str_push(&hash, "quote\"test");

	assert(hash_str_contains(&hash, "hello world"));
	assert(hash_str_contains(&hash, "tab\there"));
	assert(hash_str_contains(&hash, "newline\n"));

	hash_str_free(&hash);
	printf("✓ Passed\n\n");
}

void test_capacity_edge_cases()
{
	printf("Test 10: Capacity 1 edge case\n");
	HashStr hash = hash_str_new(1);

	hash_str_push(&hash, "a");
	hash_str_push(&hash, "b"); // Should trigger resize immediately

	assert(hash_str_contains(&hash, "a"));
	assert(hash_str_contains(&hash, "b"));

	hash_str_free(&hash);
	printf("✓ Passed\n\n");
}

int main()
{
	printf("=== Hash String Table Tests ===\n\n");

	test_basic_operations();
	test_collision_handling();
	test_resize_trigger();
	test_remove_operations();
	test_remove_from_chain();
	test_clone();
	test_empty_string();
	test_large_scale();
	test_special_characters();
	test_capacity_edge_cases();

	printf("=== All Tests Passed ===\n");
	return 0;
}
