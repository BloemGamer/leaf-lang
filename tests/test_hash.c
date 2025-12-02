#include "utils/hash.h"
#include <stdio.h>

#define ASSERT(condition, message)                                                                    \
	do                                                                                                \
	{                                                                                                 \
		if (!(condition))                                                                             \
		{                                                                                             \
			(void)fprintf(stderr, "ASSERTION FAILED: %s\n  at %s:%d\n", message, __FILE__, __LINE__); \
			return 1;                                                                                 \
		}                                                                                             \
	} while (0)

#define ASSERT_EQ(actual, expected, message) ASSERT((actual) == (expected), message)

// Test: Basic insert and contains
int test_basic_operations(void)
{
	HashStr hash = hash_str_new(8);

	ASSERT_EQ(hash_str_push(&hash, "hello"), false, "First insert should return false");
	ASSERT_EQ(hash_str_contains(&hash, "hello"), true, "Should contain inserted element");
	ASSERT_EQ(hash.size, 1, "Size should be 1");

	ASSERT_EQ(hash_str_push(&hash, "hello"), true, "Duplicate insert should return true");
	ASSERT_EQ(hash.size, 1, "Size shouldn't change on duplicate");

	hash_str_free(&hash);
	return 0;
}

// Test: Collision handling
int test_collision_handling(void)
{
	HashStr hash = hash_str_new(4);

	hash_str_push(&hash, "test1");
	hash_str_push(&hash, "test2");
	hash_str_push(&hash, "test3");

	ASSERT(hash_str_contains(&hash, "test1"), "Should contain test1");
	ASSERT(hash_str_contains(&hash, "test2"), "Should contain test2");
	ASSERT(hash_str_contains(&hash, "test3"), "Should contain test3");
	ASSERT_EQ(hash.size, 3, "Size should be 3");

	hash_str_free(&hash);
	return 0;
}

// Test: Resize trigger (CRITICAL BUG TEST)
int test_resize_trigger(void)
{
	HashStr hash = hash_str_new(4);

	hash_str_push(&hash, "item1");
	hash_str_push(&hash, "item2");
	hash_str_push(&hash, "item3");

	ASSERT(hash_str_contains(&hash, "item1"), "Should contain item1 after resize");
	ASSERT(hash_str_contains(&hash, "item2"), "Should contain item2 after resize");
	ASSERT(hash_str_contains(&hash, "item3"), "Should contain item3 after resize");

	hash_str_free(&hash);
	return 0;
}

// Test: Remove operations
int test_remove_operations(void)
{
	HashStr hash = hash_str_new(8);

	hash_str_push(&hash, "remove_me");
	hash_str_push(&hash, "keep_me");

	ASSERT_EQ(hash_str_remove(&hash, "remove_me"), true, "Remove should return true");
	ASSERT_EQ(hash_str_contains(&hash, "remove_me"), false, "Should not contain removed element");
	ASSERT(hash_str_contains(&hash, "keep_me"), "Should still contain other elements");
	ASSERT_EQ(hash.size, 1, "Size should be 1 after removal");

	ASSERT_EQ(hash_str_remove(&hash, "never_existed"), false, "Remove non-existent should return false");
	ASSERT_EQ(hash.size, 1, "Size shouldn't change on failed removal");

	hash_str_free(&hash);
	return 0;
}

// Test: Remove from collision chain
int test_remove_from_chain(void)
{
	HashStr hash = hash_str_new(2);

	hash_str_push(&hash, "a");
	hash_str_push(&hash, "b");
	hash_str_push(&hash, "c");

	hash_str_remove(&hash, "b");

	ASSERT(hash_str_contains(&hash, "a"), "Should still contain a");
	ASSERT_EQ(hash_str_contains(&hash, "b"), false, "Should not contain removed b");
	ASSERT(hash_str_contains(&hash, "c"), "Should still contain c");

	hash_str_free(&hash);
	return 0;
}

// Test: Clone operation
int test_clone(void)
{
	HashStr hash = hash_str_new(8);

	hash_str_push(&hash, "one");
	hash_str_push(&hash, "two");
	hash_str_push(&hash, "three");

	HashStr cloned = hash_str_clone(&hash);

	ASSERT_EQ(cloned.size, hash.size, "Clone should have same size");
	ASSERT_EQ(cloned.cap, hash.cap, "Clone should have same capacity");
	ASSERT(hash_str_contains(&cloned, "one"), "Clone should contain one");
	ASSERT(hash_str_contains(&cloned, "two"), "Clone should contain two");
	ASSERT(hash_str_contains(&cloned, "three"), "Clone should contain three");

	hash_str_push(&hash, "four");
	ASSERT(hash_str_contains(&hash, "four"), "Original should contain four");
	ASSERT_EQ(hash_str_contains(&cloned, "four"), false, "Clone should not contain four");

	hash_str_free(&hash);
	hash_str_free(&cloned);
	return 0;
}

// Test: Empty string handling
int test_empty_string(void)
{
	HashStr hash = hash_str_new(8);

	hash_str_push(&hash, "");
	ASSERT(hash_str_contains(&hash, ""), "Should contain empty string");
	ASSERT_EQ(hash.size, 1, "Size should be 1");

	hash_str_remove(&hash, "");
	ASSERT_EQ(hash_str_contains(&hash, ""), false, "Should not contain empty string after removal");

	hash_str_free(&hash);
	return 0;
}

// Test: Large scale operations (stress test)
int test_large_scale(void)
{
	HashStr hash = hash_str_new(8);
	char buffer[100];

	for (int i = 0; i < 1000; i++)
	{
		(void)sprintf(buffer, "item_%d", i);
		hash_str_push(&hash, buffer);
	}

	ASSERT_EQ(hash.size, 1000, "Should contain 1000 items");

	for (int i = 0; i < 1000; i++)
	{
		(void)sprintf(buffer, "item_%d", i);
		ASSERT(hash_str_contains(&hash, buffer), "Should contain all inserted items");
	}

	for (int i = 0; i < 500; i++)
	{
		(void)sprintf(buffer, "item_%d", i);
		hash_str_remove(&hash, buffer);
	}

	ASSERT_EQ(hash.size, 500, "Should contain 500 items after removal");

	hash_str_free(&hash);
	return 0;
}

// Test: Special characters
int test_special_characters(void)
{
	HashStr hash = hash_str_new(8);

	hash_str_push(&hash, "hello world");
	hash_str_push(&hash, "tab\there");
	hash_str_push(&hash, "newline\n");
	hash_str_push(&hash, "quote\"test");

	ASSERT(hash_str_contains(&hash, "hello world"), "Should contain string with space");
	ASSERT(hash_str_contains(&hash, "tab\there"), "Should contain string with tab");
	ASSERT(hash_str_contains(&hash, "newline\n"), "Should contain string with newline");

	hash_str_free(&hash);
	return 0;
}

// Test: Capacity 1 edge case
int test_capacity_edge_cases(void)
{
	HashStr hash = hash_str_new(1);

	hash_str_push(&hash, "a");
	hash_str_push(&hash, "b");

	ASSERT(hash_str_contains(&hash, "a"), "Should contain a");
	ASSERT(hash_str_contains(&hash, "b"), "Should contain b");

	hash_str_free(&hash);
	return 0;
}

// Main test runner
int main(void)
{
	int failed = 0;
	int total = 0;

#define RUN_TEST(test)                   \
	do                                   \
	{                                    \
		total++;                         \
		printf("Running %s... ", #test); \
		if (test() == 0)                 \
		{                                \
			printf("PASSED\n");          \
		}                                \
		else                             \
		{                                \
			printf("FAILED\n");          \
			failed++;                    \
		}                                \
	} while (0)

	RUN_TEST(test_basic_operations);
	RUN_TEST(test_collision_handling);
	RUN_TEST(test_resize_trigger);
	RUN_TEST(test_remove_operations);
	RUN_TEST(test_remove_from_chain);
	RUN_TEST(test_clone);
	RUN_TEST(test_empty_string);
	RUN_TEST(test_large_scale);
	RUN_TEST(test_special_characters);
	RUN_TEST(test_capacity_edge_cases);

	printf("\n========================================\n");
	printf("Tests run: %d\n", total);
	printf("Tests passed: %d\n", total - failed);
	printf("Tests failed: %d\n", failed);
	printf("========================================\n");

	return failed > 0 ? 1 : 0;
}
