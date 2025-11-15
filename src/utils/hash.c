#include <stdlib.h>
#include <string.h>

#include "utils.h"
#include "utils/hash.h"

static usize hash_str_hash(const char* str);
static void hash_str_free_node(HashStrNode* node);
static void hash_str_resize(Hash* hash, usize new_cap);

bool hash_str_contains(const Hash* hash, const char* str)
{
	usize bucket = hash_str_hash(str) % hash->cap;

	HashStrNode* node = &hash->node[bucket];
	if (node->str == NULL)
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

bool hash_str_push(Hash* hash, const char* str)
{
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
		if (node->next == NULL)
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

bool hash_str_remove(Hash* hash, const char* str)
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

Hash hash_str_clone(const Hash* hash)
{
	Hash new_hash = {.cap = hash->cap, .size = hash->size, .node = calloc(hash->cap, sizeof(HashStrNode))};

	for (usize i = 0; i < hash->cap; i++)
	{
		HashStrNode* old = &hash->node[i];
		HashStrNode* new = &new_hash.node[i];

		if (old->str == NULL)
		{
			continue;
		}

		// Clone head
		new->str = strdup(old->str);
		new->next = nullptr;

		// Clone chain
		HashStrNode* tail = new;

		while (old->next != NULL) // NOLINT
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

void hash_str_free(Hash* hash)
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

static void hash_str_resize(Hash* hash, usize new_cap)
{
	Hash new_hash = {.cap = new_cap, .size = 0, .node = calloc(new_cap, sizeof(HashStrNode))};

	// re-insert all keys
	for (usize i = 0; i < hash->cap; i++)
	{
		HashStrNode* node = &hash->node[i];

		if (node->str == NULL)
		{
			continue;
		}

		while (node) // NOLINT
		{
			// put into new table
			hash_str_push(&new_hash, node->str);
			node = node->next;
		}
	}

	// free old table
	hash_str_free(hash);

	// move fields
	*hash = new_hash;
}

/// just a copy of the djb2 algoritm, probably will try to improve this one, but works for now
static usize hash_str_hash(const char* str)
{
	unsigned long hash = 5381;
	usize c; // NOLINT

	while (c = (usize)(unsigned char)*str++) // NOLINT
		hash = ((hash << 5U) + hash) + c;	 /* hash * 33 + c */

	return hash;
}
