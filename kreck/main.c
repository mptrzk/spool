#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <unistd.h>


typedef struct Cell {
	struct Cell* car;
	struct Cell* cdr;
} Cell;

typedef Cell* Root;

typedef struct {
	Root* root;
	char* str;
} Parse;

int heap_cap;
int rstack_cap;
Cell* heap1;
Cell* heap2;
Cell* heap_bot;
Cell* heap_top;
Root* rstack_bot;
Root* rstack_top;
Root* deflist;

//sentinel values
Cell atom_sen[1];
Cell copied_sen[1];

uint64_t heap_sz() {
	return heap_top - heap_bot;
}

uint64_t rstack_sz() {
	return rstack_top - rstack_bot;
}

void heap_swap() {
	if (heap_bot == heap1) {
		heap_bot = heap_top = heap2;
	}
	else {
		heap_bot = heap_top = heap1;
	}
}

void gc();
Cell* cell_new(Cell* a, Cell* b) {
	if (heap_top == heap_bot + heap_cap) gc();
	heap_top->car = a;
	heap_top->cdr = b;
	Cell* ret = heap_top;
	heap_top++;
	return ret;
}

//should be impossible to overflow
Cell* cell_copy(Cell* cell) {
	if (!cell) return 0; //TODO test, check without
	if (cell->cdr == copied_sen) return cell->car;
	Cell* ret;
	if (cell->cdr == atom_sen) {
		ret = cell_new(cell->car, cell->cdr);
	} else {
		ret = cell_new(cell_copy(cell->car), cell_copy(cell->cdr));
	}
	cell->car = ret;
	cell->cdr = copied_sen;
	return ret;
}

void gc() {
	heap_swap();
	for (Root* r = rstack_bot; r != rstack_top; r++) {
		*r = cell_copy(*r);
	}
	if (heap_top == heap_bot + heap_cap) {
		printf("can't alloc - both heaps full\n");
		exit(-1);
	}
}


Root* cons(Root* a, Root* b) {
	*rstack_top = cell_new(*a, *b);
	Root* ret = rstack_top;
	rstack_top++;
	return ret;
}

Root* nil() {
	*rstack_top = 0;
	Root* ret = rstack_top;
	rstack_top++;
	return ret;
}

Root* atom(uint64_t value) {
	*rstack_top = cell_new((Cell*) value, atom_sen);
	Root* ret = rstack_top;
	rstack_top++;
	return ret;
}

Root* frame_return(Root* frame, Root* val) {
	rstack_top = frame + 1;
	*frame = *val;
	return frame;
}


char* skip_whitespace(char* a) {
	for (; *a && *a <= ' '; a++);
	return a;
}

Parse make_parse(Root* root, char* str) {
	Parse ret = {root, str};
	return ret;
}

int is_digit(char c) {
	return c >= '0' && c <= '9';
}

Parse parse_num(char* str) {
	uint64_t num = 0;
	for (; is_digit(str[0]); str++) num = num * 10 + str[0] - '0';
	return make_parse(atom(num), str);
}

Parse parse_expr(char* str);
Parse parse_list(char* str) {
	Root* frame = rstack_top;
	if (!str[0]) {
		printf("error - trying to parse empty expr\n");
		exit(-1);
	}
	str = skip_whitespace(str);
	if (str[0] == ')') return make_parse(nil(), str + 1);
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
		return make_parse(res.root, str + 1);
	}
	//frame_return for regularity?
	Parse parse_car = parse_expr(str);
	Parse parse_cdr = parse_list(parse_car.str);
	Root* ret = cons(parse_car.root, parse_cdr.root);
	return make_parse(frame_return(frame, ret), parse_cdr.str);
}

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
	//printf("error - trying to parse invalid atom %s, str\n"); //leak!!! use Wall
	printf("error - trying to parse invalid atom: \"%s\"\n", str);
	exit(-1);
	return make_parse(0, 0);
}

Root* rread(char* str) {
	Parse p = parse_expr(str);
	if (p.str[0]) {
		printf("error - expression %s contains trailing chars: %s\n",str, p.str);
		exit(-1);
	}
	return p.root;
}

int atomp(Cell* a) {
	return !a || a->cdr == atom_sen;
}

void cell_write(Cell* a, FILE* f);
void cell_write_list(Cell* a, FILE* f) {
	cell_write(a->car, f);
	if (atomp(a->cdr)) {
		if (a->cdr) {
			fprintf(f, " . ");
			cell_write(a->cdr, f);
		}
	} else {
		fprintf(f, " ");
		cell_write_list(a->cdr, f);
	}
}

void cell_write(Cell* a, FILE* f) {
	if (atomp(a)) {
		fprintf(f, "%ld", (uint64_t) a->car);
	} else {
		fprintf(f, "(");
		if (a) cell_write_list(a, f);
		fprintf(f, ")");
	}
}

void writenl(Root* a) {
	cell_write(*a, stdout);
	putchar('\n');
}

//shouldn't writing functions take roots?
// no, they are recursive, and there are cells that aren't roots
//TODO ponder cell funs and root funs

void init(int heap_size, int rstack_size) {
	heap_cap = heap_size;
	heap1 = malloc(heap_cap * sizeof(Cell));
	heap2 = malloc(heap_cap * sizeof(Cell));
	heap_top = heap_bot = heap1;
	rstack_cap = rstack_size;
	rstack_bot = malloc(rstack_cap * sizeof(Root));
	rstack_top = rstack_bot;
	deflist = nil(); //not nil root? TODO
} //gc and nil!!!

void deinit() {
	free(heap1);
	free(heap2);
	free(rstack_bot);
}

void def_add(char* name, Root* root) {
	Root* frame = rstack_top;
	Root* new_def = cons(atom((uint64_t) name), root);
	deflist = frame_return(frame, cons(new_def, deflist));
}

Root* car(Root* a) {
	*rstack_top = (*a)->car;
	Root* ret = rstack_top;
	rstack_top++;
	return ret;
}

Root* cdr(Root* a) {
	*rstack_top = (*a)->cdr;
	Root* ret = rstack_top;
	rstack_top++;
	return ret;
}


#include "tests.h"

int main() {
	tests();
	init(100, 100);
	deinit();
}

