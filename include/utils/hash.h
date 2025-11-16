#include "utils.h"

typedef struct HashStrNode HashStrNode;

struct HashStrNode // NOLINT
{
	const char* str;
	HashStrNode* next;
};

typedef struct // NOLINT
{
	HashStrNode* node;
	usize size;
	usize cap;
} HashStr;

bool hash_str_contains(const HashStr* hash, const char* str);

/// returns true if the string was already in the hash, returns fals otherwise
bool hash_str_push(HashStr* hash, const char* str);
bool hash_str_remove(HashStr* hash, const char* str);
HashStr hash_str_clone(const HashStr* hash);
void hash_str_free(HashStr* hash);
HashStr hash_str_new(usize cap);
