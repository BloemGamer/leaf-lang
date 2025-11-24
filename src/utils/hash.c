#include <stdlib.h>
#include <string.h>

#include "assert.h"
#include "basic_types.h"
#include "utils/hash.h"

static usize hash_str_hash(const char* str);
static void hash_str_free_node(HashStrNode* node);
static void hash_str_resize(HashStr* hash, usize new_cap);

bool hash_str_contains(const HashStr* hash, const char* str)
{
	if (str == nullptr)
	{
		return false;
	}
	usize bucket = hash_str_hash(str) % hash->cap;

	HashStrNode* node = &hash->node[bucket];
	if (node->str == nullptr)
	{
		return false;
	}

	while (node) // NOLINT
	{
		if (node->str && strcmp(node->str, str) == 0)
		{
			return true;
		}
		node = node->next;
	}
	return false;
}

bool hash_str_push(HashStr* hash, const char* str) // NOLINT
{
	if (hash->size * 2 >= hash->cap)
	{
		hash_str_resize(hash, hash->cap * 2);
	}

	usize bucket = hash_str_hash(str) % hash->cap;
	HashStrNode* node = &hash->node[bucket];

	if (node->str == nullptr)
	{
		node->str = strdup(str);
		node->next = nullptr;
		hash->size++;
		return false;
	}

	while (node) // NOLINT
	{
		if (strcmp(node->str, str) == 0)
		{
			return true;
		}
		if (node->next == nullptr)
		{
			HashStrNode* new_node = malloc(sizeof(HashStrNode));
			new_node->str = strdup(str);
			new_node->next = nullptr;

			node->next = new_node;
			hash->size++;
			return false;
		}
		node = node->next;
	}
	return true;
}

bool hash_str_remove(HashStr* hash, const char* str)
{
	usize bucket = hash_str_hash(str) % hash->cap;

	HashStrNode* node = &hash->node[bucket];
	HashStrNode* prev = nullptr;

	if (node->str == nullptr)
	{
		return false;
	}

	while (node) // NOLINT
	{
		if (strcmp(node->str, str) == 0)
		{
			if (prev == nullptr)
			{
				if (node->next == nullptr)
				{
					// Leave bucket as empty node
					free((void*)node->str);
					node->str = nullptr;
				}
				else
				{
					HashStrNode* next = node->next;

					free((void*)node->str);

					node->str = next->str;
					node->next = next->next;

					free(next);
				}
			}
			else
			{
				prev->next = node->next;
				free((void*)node->str);
				free(node);
			}

			hash->size--;
			return true;
		}

		prev = node;
		node = node->next;
	}
	return false;
}

HashStr hash_str_clone(const HashStr* hash)
{
	HashStr new_hash = {.cap = hash->cap, .size = hash->size, .node = calloc(hash->cap, sizeof(HashStrNode))};

	for (usize i = 0; i < hash->cap; i++)
	{
		HashStrNode* old = &hash->node[i];
		HashStrNode* new = &new_hash.node[i];

		if (old->str == nullptr)
		{
			continue;
		}

		new->str = strdup(old->str);
		new->next = nullptr;

		HashStrNode* tail = new;

		while (old->next != nullptr) // NOLINT
		{
			old = old->next;

			HashStrNode* copy = malloc(sizeof(HashStrNode));
			copy->str = strdup(old->str);
			copy->next = nullptr;

			tail->next = copy;
			tail = copy;
		}
	}

	return new_hash;
}

static void hash_str_free_node(HashStrNode* node)
{
	if (node->str)
	{
		free((void*)node->str);
		node->str = nullptr;
	}

	HashStrNode* cur = node->next;

	while (cur != nullptr) // NOLINT
	{
		HashStrNode* next = cur->next;
		free((void*)cur->str);
		free(cur);
		cur = next;
	}

	node->next = nullptr;
}

void hash_str_free(HashStr* hash)
{
	if (!hash || !hash->node)
	{
		return;
	}

	for (usize i = 0; i < hash->cap; i++) // NOLINT
	{
		hash_str_free_node(&hash->node[i]);
	}

	free(hash->node);
	hash->node = nullptr;
	hash->size = 0;
	hash->cap = 0;
}

static void hash_str_resize(HashStr* hash, usize new_cap)
{
	HashStrNode* new_nodes = calloc(new_cap, sizeof(HashStrNode));

	for (usize i = 0; i < hash->cap; i++)
	{
		HashStrNode* node = &hash->node[i];

		if (node->str == nullptr)
		{
			continue;
		}

		while (node) // NOLINT
		{
			usize new_bucket = hash_str_hash(node->str) % new_cap;
			HashStrNode* new_node = &new_nodes[new_bucket];

			if (new_node->str == nullptr)
			{
				new_node->str = strdup(node->str);
				new_node->next = nullptr;
			}
			else
			{
				while (new_node->next != nullptr) // NOLINT
				{
					new_node = new_node->next;
				}

				HashStrNode* chain_node = malloc(sizeof(HashStrNode));
				chain_node->str = strdup(node->str);
				chain_node->next = nullptr;
				new_node->next = chain_node;
			}

			node = node->next;
		}
	}

	hash_str_free(hash);

	hash->cap = new_cap;
	hash->node = new_nodes;

	hash->size = 0;
	for (usize i = 0; i < new_cap; i++)
	{
		HashStrNode* node = &new_nodes[i];
		if (node->str != nullptr)
		{
			hash->size++;
			node = node->next;
			while (node != nullptr) // NOLINT
			{
				hash->size++;
				node = node->next;
			}
		}
	}
}

/// djb2 algorithm
static usize hash_str_hash(const char* str)
{
	unsigned long hash = 5381;
	usize c; // NOLINT

	while (c = (usize)(unsigned char)*str++) // NOLINT
		hash = ((hash << 5U) + hash) + c;	 /* hash * 33 + c */

	return hash;
}

HashStr hash_str_new(const usize cap)
{
	debug_assert(cap > 0);
	HashStr hash = {.cap = cap, .size = 0, .node = calloc(cap, sizeof(HashStrNode))};
	return hash;
}
