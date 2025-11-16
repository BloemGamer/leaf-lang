#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// include your full hash implementation here
// or include your header instead:
#include "utils/hash.h"

// ------------------------------------------------------------
// Test: push + contains
// ------------------------------------------------------------
static void test_push_contains()
{
	printf("TEST: push + contains...\n");

	HashStr h = hash_str_new(8);

	assert(hash_str_contains(&h, "apple") == false);

	assert(hash_str_push(&h, "apple") == false);
	assert(hash_str_contains(&h, "apple") == true);

	// duplicates should be rejected
	assert(hash_str_push(&h, "apple") == true);

	assert(hash_str_push(&h, "banana") == false);
	assert(hash_str_push(&h, "orange") == false);

	assert(hash_str_contains(&h, "banana") == true);
	assert(hash_str_contains(&h, "orange") == true);

	assert(h.size == 3);

	hash_str_free(&h);

	printf("OK\n");
}

// ------------------------------------------------------------
// Test: remove
// ------------------------------------------------------------
static void test_remove()
{
	printf("TEST: remove...\n");

	HashStr h = hash_str_new(8);

	hash_str_push(&h, "a");
	hash_str_push(&h, "b");
	hash_str_push(&h, "c");

	assert(h.size == 3);

	assert(hash_str_remove(&h, "b") == true);
	assert(hash_str_contains(&h, "b") == false);
	assert(h.size == 2);

	// removing again should fail
	assert(hash_str_remove(&h, "b") == false);

	// removing head nodes
	assert(hash_str_remove(&h, "a") == true);
	assert(hash_str_remove(&h, "c") == true);

	assert(h.size == 0);

	// table is now empty
	assert(hash_str_contains(&h, "a") == false);

	hash_str_free(&h);

	printf("OK\n");
}

// ------------------------------------------------------------
// Test: clone
// ------------------------------------------------------------
static void test_clone()
{
	printf("TEST: clone...\n");

	HashStr h = hash_str_new(8);

	hash_str_push(&h, "car");
	hash_str_push(&h, "train");
	hash_str_push(&h, "plane");

	HashStr c = hash_str_clone(&h);

	// Same values
	assert(c.cap == h.cap);
	assert(c.size == h.size);

	assert(hash_str_contains(&c, "car"));
	assert(hash_str_contains(&c, "train"));
	assert(hash_str_contains(&c, "plane"));

	// Deep copy: editing one doesn't affect other
	hash_str_remove(&h, "car");
	assert(hash_str_contains(&h, "car") == false);
	assert(hash_str_contains(&c, "car") == true);

	// Free both
	hash_str_free(&h);
	hash_str_free(&c);

	printf("OK\n");
}

// ------------------------------------------------------------
// Test: free
// ------------------------------------------------------------
static void test_free()
{
	printf("TEST: free...\n");

	HashStr h = hash_str_new(8);

	hash_str_push(&h, "x");
	hash_str_push(&h, "y");
	hash_str_push(&h, "z");

	assert(h.size == 3);

	hash_str_free(&h);

	// Structure must now be reset
	assert(h.node == NULL);
	assert(h.cap == 0);
	assert(h.size == 0);

	printf("OK\n");
}

// ------------------------------------------------------------
// Main: run all tests
// ------------------------------------------------------------
int main(void)
{
	test_push_contains();
	test_remove();
	test_clone();
	// test_resize();
	test_free();

	printf("\nALL TESTS PASSED ✔\n");
	return 0;
}
