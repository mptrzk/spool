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

typedef struct {
	Root* root;
	int tail;
} Opval;

typedef Opval (*Op)(Root* subj, Root* args);

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
int dbg = 0;

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
	if (heap_top == heap_bot + heap_cap) gc(); //corrupts arguments?
	heap_top->car = a;
	heap_top->cdr = b;
	Cell* ret = heap_top;
	heap_top++;
	return ret;
}

//should be impossible to overflow
Cell* cell_copy(Cell* cell) {
	if (!cell) return 0;
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
	if (heap_top >= heap_bot + heap_cap) {
		printf("can't alloc - both heaps full\n");
		exit(-1);
	}
}

Root* make_root(Cell* a) {
	*rstack_top = a;
	Root* ret = rstack_top;
	rstack_top++;
	return ret;
}

Root* cons(Root* a, Root* b) {
	return make_root(cell_new(*a, *b));
}

Root* nil() { //TODO to make root
	return make_root(0);
}

Root* atom(uint64_t value) {
	return make_root(cell_new((Cell*) value, atom_sen));
}

Root* make_frame() {
	return rstack_top;
}

Root* frame_return(Root* frame, Root* val) {
	rstack_top = frame + 1;
	*frame = *val;
	return frame;
}

void frame_restore(Root* frame) {
	rstack_top = frame;
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

void def_add(char* name, Root* root) {
	Root* frame = make_frame();
	Root* new_def = cons(atom((uint64_t) name), root);
	deflist = frame_return(frame, cons(new_def, deflist));
}
//getting rid of old deflist roots
//init_defs?

int word_equal(char* a, char* b) {
	int i;
	for (i=0; is_word_char(a[i]); i++) {
		if (a[i] != b[i]) return 0;
	}
	return !is_word_char(b[i]);
}

Root* def_get(char* name) {
	for (Cell* d = *deflist; d; d = d->cdr) {
		if (word_equal(name, (char*) d->car->car->car)) return make_root(d->car->cdr);
	}
	return nil();
}

Parse parse_expr(char* str);
Parse parse_list(char* str) {
	Root* frame = make_frame();
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
	Root* def = def_get(str);
	return make_parse(def, skip_word(str));
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

int listp(Cell* a) {
	return !a || a->cdr != atom_sen;
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
	if (!listp(a)) {
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




Root* car(Root* a) {
	if (*a == 0) return nil();
	if (atomp(*a)) {// better predicate
		printf("car - argument not a list\n");
		exit(-1);
	}
	return make_root((*a)->car);
}

Root* cdr(Root* a) {
	if (*a == 0) return nil();
	if (atomp(*a)) {// better predicate
		printf("cdr - argument not a list\n");
		exit(-1);
	}
	return make_root((*a)->cdr);
}



int cell_iden(Cell* a, Cell* b) {
	if (a == b) return 1; 
	if (!a || !b) return 0; //nil
	if (atomp(a) || atomp(b)) {  //TODO ponder
		return (atomp(a) && atomp(b)) ? a->car == b->car : 0;
	}
	return (cell_iden(a->car, b->car) && cell_iden(a->cdr, b->cdr));
}

int iden(Root* a, Root* b) {
	return cell_iden(*a, *b);
}

Opval make_opval(Root* root, int tail) {
	Opval ret = {root, tail};
	return ret;
}


Root* kreck_eval(Root* subj, Root* form);
Root* kreck_evlist(Root* subj, Root* forms) {
	if (*forms == 0) return forms;
	Root* frame = make_frame();
	Root* ret = cons(kreck_eval(subj, car(forms)), kreck_evlist(subj, cdr(forms)));
	return frame_return(frame, ret);
}

Root* kreck_eval(Root* subj, Root* form) {
	Root* frame = make_frame();
	int ctr = 0;
	while(1) {
		//gc();
		if (dbg) printf("kreck %d %d %d!\n", ctr++, heap_sz(), rstack_sz());
		Root* op = car(form); //TODO should those be cells?
		Root* args = cdr(form);
		if (atomp(*op)) {
			Op f = (Op) (*op)->car;
			Opval opval = f(subj, args);
			Root* ret = opval.root;
			ret = frame_return(frame, ret); //TODO elaborate on that bug
			if (!opval.tail) return ret;
			subj = car(ret);
			form = cdr(ret);
			continue;
		}
		Root* eform = kreck_evlist(subj, form);
		Root* clos = car(eform);
		Root* eargs = cdr(eform);
		Root* newsubj = cons(eargs, cdr(clos));
		Root* newform = car(clos);
		Root* tcall = frame_return(frame, cons(newsubj, newform));
		subj = car(tcall);
		form = cdr(tcall);
	}
}

Root* kreck(char* subj, char* form) {
	Root* frame = make_frame();
	return frame_return(frame , kreck_eval(rread(subj), rread(form)));
}

Opval subj_op(Root* subj, Root* args) {
	return make_opval(subj, 0);
}

Opval quot_op(Root* subj, Root* args) {
	return make_opval(car(args), 0);
}

Opval car_op(Root* subj, Root* args) {
	Root* frame = make_frame();
	Root* earg = kreck_eval(subj, car(args));
	Root* ret = frame_return(frame, car(earg));
	return make_opval(ret, 0);
}

Opval cdr_op(Root* subj, Root* args) {
	Root* frame = make_frame();
	Root* earg = kreck_eval(subj, car(args));
	Root* ret = frame_return(frame, cdr(earg));
	return make_opval(ret, 0);
}

Opval cons_op(Root* subj, Root* args) {
	Root* frame = make_frame();
	Root* earg1 = kreck_eval(subj, car(args));
	Root* earg2 = kreck_eval(subj, car(cdr(args)));
	Root* ret = frame_return(frame, cons(earg1, earg2));
	return make_opval(ret, 0);
}

Opval eval_op(Root* subj, Root* args) {
	Opval ret = cons_op(subj, args);
	return make_opval(ret.root, 1);
}

Opval cond_op(Root* subj, Root* args) {
	Root* frame = make_frame();
	Root* earg1 = kreck_eval(subj, car(args));
	Root* arg2 = car(cdr(args));
	Root* arg3 = car(cdr(cdr(args)));
	Root* ret = frame_return(frame, cons(subj, *earg1 ? arg2 : arg3));
	return make_opval(ret, 1);
}

Opval list_op(Root* subj, Root* args) {
	return make_opval(kreck_evlist(subj, args), 0);
}

Opval macro_op(Root* subj, Root* args) {
	Root* margs = cdr(args);
	Root* clos = kreck_eval(subj, car(args));
	Root* newsubj = cons(margs, cdr(clos));
	Root* newform = car(clos); //better names
	Root* ret = cons(subj, kreck_eval(newsubj, newform));
	return make_opval(ret, 1);
}

//TODO ponder necessity/redundancy of frames in ops
//tail calls?
//kreck collects stuff so it shouldn't matter
//how about reading functions?


void init_defs() {
	Root* frame = make_frame();
	def_add("~", nil());
	def_add("$", atom((uint64_t) subj_op));
	def_add("'", atom((uint64_t) quot_op));
	def_add("<", atom((uint64_t) car_op));
	def_add(">", atom((uint64_t) cdr_op));
	def_add(":", atom((uint64_t) cons_op));
	def_add("*", atom((uint64_t) eval_op));
	def_add("?", atom((uint64_t) cond_op));
	def_add("::", atom((uint64_t) list_op));
	def_add("!", atom((uint64_t) macro_op));
	deflist = frame_return(frame, deflist); //TODO elaborate!
}

void init(int heap_size, int rstack_size) {
	heap_cap = heap_size;
	heap1 = malloc(heap_cap * sizeof(Cell));
	heap2 = malloc(heap_cap * sizeof(Cell));
	heap_top = heap_bot = heap1;
	rstack_cap = rstack_size;
	rstack_bot = malloc(rstack_cap * sizeof(Root));
	rstack_top = rstack_bot;
	deflist = nil(); //not nil root? TODO
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
	deinit();
}

