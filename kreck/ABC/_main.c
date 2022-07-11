#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <unistd.h>
#include <assert.h>


typedef struct Noun {
	struct Noun* car;
	struct Noun* cdr;
} Noun;

typedef struct {
	int cap;
	Noun* bot;
	Noun* top;
} Heap;

typedef struct {
	Noun* noun;
	char* str;
} Parse;

Heap heap;
Noun word_sen[1];
int nogc = 0;
int dbg = 0;

Noun* deflist;

uint64_t heap_sz() {
	return heap.top - heap.bot;
}

Noun* car(Noun* noun) {
	return noun->car;
}

Noun* cdr(Noun* noun) {
	return noun->cdr;
}


Noun* cons(Noun* a, Noun* b) {
	Noun* ret = heap.top;
	ret->car = a;	
	ret->cdr = b;	
	heap.top++;
	assert(heap_sz() <= heap.cap);
	return ret; 
}

Noun* word(uint64_t val) {
	return cons((Noun*) val, word_sen);
}

//TODO consider renaming
uint64_t word_val(Noun* word) {
	return (uint64_t) word->car;
}

int wordp(Noun* noun) {
	return noun && noun->cdr == word_sen;
}

int listp(Noun* noun) {
	return !noun || noun->cdr != word_sen;
}

int atomp(Noun* noun) {
	return !noun || noun->cdr == word_sen;
}


int iden(Noun* a, Noun* b) {
	if (a == b) return 1; 
	if (!a || !b) return 0; //nil
	if (wordp(a) || wordp(b)) {  //TODO ponder
		return (wordp(a) && wordp(b)) ? a->car == b->car : 0;
	}
	return (iden(a->car, b->car) && iden(a->cdr, b->cdr));
}

Noun* make_frame() {
	return heap.top;
}



Noun* noun_copy(Noun* noun, Noun* frame, Noun* copy_buf) { 
	static Noun copied_sen[1];
	if (!noun) return 0;
	if (noun - heap.bot < frame - heap.bot) return noun; 
	if (noun->cdr == copied_sen) return noun->car;
	int offset = copy_buf - frame;
	Noun* ret;
	if (noun->cdr == word_sen) ret = cons(noun->car, word_sen);
	else ret = cons(noun_copy(noun->car, frame, copy_buf),
									noun_copy(noun->cdr, frame, copy_buf)); 
	ret -= offset;
	noun->car = ret;
	noun->cdr = copied_sen;
	return ret;
}

Noun* gc(Noun* noun, Noun* frame) {		
	if (nogc) return noun;
	Noun* copy_buf = heap.top;
	noun_copy(noun, frame, copy_buf);
	int n = heap.top - copy_buf;
	if (!n) {
		heap.top = frame;
		return noun;
	}
	for (int i=0; i<n; i++) {
		frame[i] = copy_buf[i];
	}
	heap.top = frame + n;
	return heap.top - 1;
}

void noun_write(Noun* a, FILE* f);
void noun_write_list(Noun* a, FILE* f) {
	noun_write(a->car, f);
	if (atomp(a->cdr)) {
		if (a->cdr) {
			fprintf(f, " . ");
			noun_write(a->cdr, f);
		}
	} else {
		fprintf(f, " ");
		noun_write_list(a->cdr, f);
	}
}

void noun_write(Noun* a, FILE* f) {
	if (dbg) printf("reached\n");
	if (wordp(a)) {
		fprintf(f, "%ld", (uint64_t) a->car);
	} else {
		fprintf(f, "(");
		if (a) noun_write_list(a, f);
		fprintf(f, ")");
	}
}

void writenl(Noun* a) {
	noun_write(a, stdout);
	putchar('\n');
}

int is_word_char(char c) {
	return c > ' ' && c != '(' && c != ')';
}

char* skip_whitespace(char* a) {
	for (; *a && *a <= ' '; a++);
	return a;
}

char* skip_word(char* a) {
	for (; is_word_char(*a); a++); //TODO '(' too?
	return a;
}

Parse make_parse(Noun* noun, char* str) {
	Parse ret = {noun, str};
	return ret;
}

int is_digit(char c) {
	return c >= '0' && c <= '9';
}

Parse parse_num(char* str) { //TODO rearrange
	uint64_t num = 0;
	for (; is_digit(str[0]); str++) num = num * 10 + str[0] - '0';
	return make_parse(word(num), str);
}

int word_equal(char* a, char* b) {
	int i;
	for (i=0; is_word_char(a[i]); i++) {
		if (a[i] != b[i]) return 0;
	}
	return !is_word_char(b[i]);
}

void def_add(char* name, Noun* noun) {
	Noun* frame = make_frame();
	Noun* new_def = cons(word((uint64_t) name), noun);
	deflist = gc(new_def, frame);
}

Noun* def_get(char* name) {
	for (Noun* d = deflist; d; d = d->cdr) {
		if (word_equal(name, (char*) d->car->car->car)) return d->car->cdr;
	}
	return 0;
}

Parse parse_expr(char* str);
Parse parse_list(char* str) {
	if (!str[0]) {
		printf("error - trying to parse empty expr\n");
		exit(-1);
	}
	str = skip_whitespace(str);
	if (str[0] == ')') return make_parse(0, str + 1);
	if (str[0] == '.') { 
		Parse res = parse_expr(str + 1);
		str = skip_whitespace(res.str);
		if (!str[0]) {
			printf("error - trying to parse empty expr\n");
			exit(-1);
		}
		if (str[0] != ')') {
			printf("error - aaaa\n");
			exit(-1);
		}
		return make_parse(res.noun, str + 1);
	}
	//frame_return for regularity?
	Parse parse_car = parse_expr(str);
	Parse parse_cdr = parse_list(parse_car.str);
	Noun* ret = cons(parse_car.noun, parse_cdr.noun);
	return make_parse(ret, parse_cdr.str); 
}
//GC is redundant here with ABC
//list reading doesn't generate garbage
//TODO explain why it isn't redundant with SnC


Parse parse_expr(char* str) {
	if (!str[0]) {
		printf("error - trying to parse empty expr\n");
		exit(-1);
	}
	str = skip_whitespace(str);
	if (str[0] == ')') {
		printf("error - unexpected ')' in %s", str);
		exit(-1);
	}
	if (str[0] == '(') return parse_list(str + 1);
	if (is_digit(str[0])) return parse_num(str);
	/*
	Noun* def = def_get(str);
	//printf("error - trying to parse invalid word %s, str\n"); //leak!!! use Wall
	if (!(*def)) {
		printf("parse_expr - definition \"%s\" doesn't exist\n", str);
		exit(-1);
	}
	return make_parse(def, skip_word(str));
	*/
	printf("not yet!\n");
	exit(-1);
}

Noun* rread(char* str) {
	Parse p = parse_expr(str);
	if (p.str[0]) {
		printf("error - expression %s contains trailing chars: %s\n",str, p.str);
		exit(-1);
	}
	return p.noun;
}


void init(int noun_cap) {
	heap.cap = noun_cap;
	heap.bot = malloc(noun_cap * sizeof(Noun));
	heap.top = heap.bot;
	deflist = 0;
}

void deinit() {
	free(heap.bot);
}

#include "tests.h"

int main() {
	tests();
}
