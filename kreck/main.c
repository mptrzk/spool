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

int heap_cap;
int rstack_cap;
Cell* heap1;
Cell* heap2;
Cell* heap_bot;
Cell* heap_top;
Root* rstack_bot;
Root* rstack_top;

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

//arguments to this shouldn't be in a heap unless they are roots
//why?
//so that GC doesn't break them
//but wouldn't it also break arguments which are roots?
//cells are passed to that function, not roots
/*
Root* cons_lit(Cell* a, Cell* b) {
	Root* ret;
	if (heap_top != heap + heap_cap) {
		heap_top->car = a;
		heap_top->cdr = b;
		*rstack_top = heap_top;
		ret = rstack_top;
		heap_top++;
		rstack_top++;
		return ret;
	}
}
*/


Cell* cell_new(Cell* a, Cell* b) {
	heap_top->car = a;
	heap_top->cdr = b;
	Cell* ret = heap_top;
	heap_top++;
	return ret;
}

//should be impossible to overflow
Cell* cell_copy(Cell* cell) {
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
	if (heap_top == heap_bot + heap_cap) {
		heap_swap();
		for (Root* r = rstack_bot; r != rstack_top; r++) {
			*r = cell_copy(*r);
		}
		if (heap_top == heap_bot + heap_cap) {
			printf("can't alloc - both heaps full\n");
			exit(-1);
		}
	}
}


Root* cons(Root* a, Root* b) {
	gc();
	*rstack_top = cell_new(*a, *b);
	Root* ret = rstack_top;
	rstack_top++;
	return ret;
}

Root* atom(uint64_t value) {
	gc();
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


/*
it looks terribly stateful
I want it to not be stateful
Root* parse_expr(char** strp);
Root* parse_list(char** strp) {
}

Root* parse_expr(char** strp) {
	if (**strp == 0) {
		printf(""
	}
	while (**strp <= ' ') (*strp)++;
}
*/

/*
Root* parse_expr(char* str, char** str_ret);
Root* parse_list(char* str, char** str_ret) {
	
}

Root* parse_expr(char* str, char** str_ret) {
	if (!str[0]) {
		printf("error - trying to parse empty expr");
		exit(-1);
	}
	if (str[0] == ')') {
		printf("error - unexpected ')' in %s", str);
		exit(-1);
	}
	if (str[0] <= ' ') return parse_expr(str + 1, str_ret);
}

Root* read(char* str) {
	char* str_left;
	Root* ret =  parse_expr(str, &str_left);
	if (str_left) {
		printf("error - expression %s contains trailing chars: %s\n");
		exit(-1);
	}
	return ret;
}
*/

int atomp(Cell* a) {
	return a->cdr == atom_sen;
}

void cell_write(Cell* a, FILE* f);
void cell_write_list(Cell* a, FILE* f) {
	cell_write(a->car, f);
	if (atomp(a->cdr)) {
		if (a->cdr->car) {
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
		fprintf(f, "%ld", a->car);
	} else {
		fprintf(f, "(");
		cell_write_list(a, f);
		fprintf(f, ")");
	}
}

void writenl(Root* a) {
	cell_write(*a, stdout);
	putchar('\n');
}
//TODO testable version for file streams

//shouldn't writing functions take roots?
// no, they are recursive, and there are cells that aren't roots
//TODO ponder cell funs and root funs

//make heap size flexible
//test - consecutive frames
void init(int heap_size, int rstack_size) {
	heap_cap = heap_size;
	heap1 = malloc(heap_cap * sizeof(Cell));
	heap2 = malloc(heap_cap * sizeof(Cell));
	heap_top = heap_bot = heap1;
	rstack_cap = rstack_size;
	rstack_bot = malloc(rstack_cap * sizeof(Root));
	rstack_top = rstack_bot;
}

void deinit() {
	free(heap1);
	free(heap2);
	free(rstack_bot);
}

#include "tests.h"

int main() {
	tests();
	init(100, 100);
	writenl(cons(atom((uint64_t) main), cons(atom(0), atom(0))));
	deinit();
}

