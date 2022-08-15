#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "reader.h"

Defs defs;
Prim prim;
Noun* lastsym;

long int defs_count() {
	return defs.last - defs.first;
}

void def_add_noun(char* name, Noun* noun) {
	defs.last->name = name;
	defs.last->noun = noun;
	defs.last++;
}

void def_add(char* name, char* expr) {
	Noun* noun = noun_read(expr);
	def_add_noun(name, noun);
}

Noun* def_get(char* name) {
	for (int i=0; i < defs.last - defs.first; i++) {
		if (word_equal(name, defs.first[i].name)) return defs.first[i].noun; 
	}
	printf("Def not found");
	return 0;
}



bool word_equal(char* a, char* b) {
	int i=0;
	for (; a[i] > ' ' && a[i] != ']'; i++) {
		if (a[i] != b[i]) return false;
	}
	if (b[i] > ' ' && b[i] != ']') return false;
	return true;
}


Noun* inc(Noun* noun) {
	Noun* frame = mem.last;
	Noun* res;
	if (!noun) res = cons(cons(0, 0), 0);
	else if (!noun->head) res = cons(cons(0, 0), noun->tail);
	else res = cons(0, inc(noun->tail));
	return gc(res, frame);
}


Noun* add(Noun* a, Noun* b) {
	Noun* frame = mem.last;
	if (!a && !b) return 0;
	if (!a) return b;
	if (!b) return a;
	Noun* first = ((a->head || b->head) && !(a->head && b->head)) ? cons(0, 0) : 0;
	Noun* rest = add(a->tail, b->tail);
	Noun* res = cons(first, (a->head && b->head) ? inc(rest) : rest);
	return gc(res, frame);
}

Noun* mul10(Noun* a) {
	return a ? add(cons(0, cons(0, cons(0, a))), cons(0, a)) : 0;
}

bool is_digit(char c) {
	return c >= '0' && c <= '9';
}


Parse parse_make(Noun* noun, char* str) {
	Parse parse = {noun, str};
	return parse;
}

Parse list_parse(char* str) {
	if (!str[0]) {
		printf("']' missing!\n");
		return parse_make(0, 0);
	}
	if (str[0] <= ' ') return list_parse(str + 1);
	if (str[0] == ']') return parse_make(0, str + 1);
	Parse car_parse = noun_parse(str);
	Parse cdr_parse = list_parse(car_parse.str);
	return parse_make(cons(car_parse.noun, cdr_parse.noun),
										cdr_parse.str);
}

Parse uint_parse(char* str) {
	Noun* frame = mem.last;
	Noun* res = 0;
	int i;
	for (i=0; is_digit(str[i]); i++) {
		res = gc(add(mul10(res), prim.digits[str[i] - '0']), frame);
	}
	return parse_make(res, str + i);
}

Parse def_parse(char* str) {
	for (int i=0; i < defs.last - defs.first; i++) {
		if (word_equal(str, defs.first[i].name)) {
			 return parse_make(defs.first[i].noun, str + strlen(defs.first[i].name));
		}
	}
	return parse_make(0, 0);
}

Parse noun_parse(char* str) {
	if (str[0] <= ' ') return noun_parse(str + 1);
	if (str[0] == '[') return list_parse(str + 1);
	if (is_digit(str[0])) return uint_parse(str);
	return def_parse(str);
}

Noun* noun_read(char* str) {
	return noun_parse(str).noun;
}


Noun* gensym() {
	lastsym = inc(lastsym);
	return lastsym;
}


void defs_init(int def_cap) {
	defs.cap = def_cap;
	defs.first = malloc(def_cap * sizeof(Def));
	defs.last = defs.first;
	char* ops[] = {"$", "'", "<", ">", ":", "=", "*", "?", "!", "::"};
	int opcount = sizeof(ops)/sizeof(char*);
	for (int i=0; i<opcount; i++) def_add_noun(ops[i], cons(0, cons(gensym(), 0)));
	def_add_noun("~", 0);
	def_add_noun("T", cons(0, 0));
	prim.digits[0] = 0;
	for (int i=1; i < 10; i++) prim.digits[i] = inc(prim.digits[i-1]);
	prim.subj = def_get("$");
	prim.quot = def_get("'");
	prim.head = def_get("<");
	prim.tail = def_get(">");
	prim.cons = def_get(":");
	prim.equl = def_get("=");
	prim.eval = def_get("*");
	prim.cond = def_get("?");
	prim.list = def_get("::");
	prim.macr = def_get("!");
}
