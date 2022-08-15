#ifndef READER_H
#define READER_H

#include <stdbool.h>
#include "noun.h"

typedef struct {
	Noun* noun;
	char* str;
} Parse;

typedef struct {
	char* name;
	Noun* noun;
} Def;

typedef struct {
	Def* first;
	Def* last;
	int cap;
} Defs;

typedef struct {
	Noun* subj;
	Noun* quot;
	Noun* equl;
	Noun* head;
	Noun* tail;
	Noun* cons;
	Noun* eval;
	Noun* cond;
	Noun* list;
	Noun* macr;
	Noun* digits[10];
} Prim;


extern Defs defs;
extern Prim prim;
extern Noun* lastsym;

Noun* inc(Noun* noun);
Noun* add(Noun* a, Noun* b);
Noun* mul10(Noun* a);
bool is_digit(char c);

void defs_init(int def_cap);
long int defs_count();
void def_add_noun(char* name, Noun* noun);
void def_add(char* name, char* expr);
Noun* def_get(char* name);

bool word_equal(char* a, char* b);
Parse parse_make(Noun* noun, char* str);
Parse noun_parse(char* str);
Parse list_parse(char* str);
Parse uint_parse(char* str);
Parse def_parse(char* str);
Parse noun_parse(char* str);
Noun* noun_read(char* str);
Noun* noun_read(char* str);
Noun* gensym();


#endif
