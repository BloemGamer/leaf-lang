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
} Hash;

bool hash_str_contains(const Hash* hash, const char* str);

/// returns true if the string was already in the hash, returns fals otherwise
bool hash_str_push(Hash* hash, const char* str);
bool hash_str_remove(Hash* hash, const char* str);
Hash hash_str_clone(const Hash* hash);
void hash_str_free(Hash* hash);
