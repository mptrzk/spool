#include <stdio.h>
#include <stdlib.h>
#include "decl.h"
#include <assert.h>



int cell_count; //TODO should it be just for debugging? Is it really useful?
int mem_size;
int last_alloc;
Cell* mem;


int max_error_count;
int error_count;
Error* errors;

int max_def_count;
int def_count;
Def* defs;

Primitives prim;


void init(int mem_size_, int max_error_count_, int max_def_count_) {
	prim.tru->head = 0;
	prim.tru->tail = 0;
	prim.tru->ref_count = 1;

	cell_count = 0;
	mem_size = mem_size_;
	last_alloc = 0;
	mem = malloc(mem_size_ * sizeof(Cell));
	for (int i=0; i<mem_size_; i++) mem[i].ref_count = 0;

	max_error_count = max_error_count_;
	error_count = 0;
	errors = malloc(max_error_count * sizeof(Error));

	max_def_count = max_def_count_;
	def_count = 0;
	defs = malloc(max_def_count * sizeof(Def));
}

void close() {
	mem_size = 0; //should I bother with zeroing stuff out?
	last_alloc = 0;
	max_error_count = 0;
	error_count = 0;
	free(mem);
	free(errors);
	free(defs);
}

void error_add(char* name, int argc) {
	Error* err = &errors[error_count];
	err->name = name;
	err->argc = argc;
	error_count++;
}

void error_print() {
	for (int i=0; i<error_count; i++) printf("%s\n", errors[i].name);
}

int is_cell(Cell* cell) {
	return (cell && cell != prim.err) ? 1 : 0;
}

Cell* attach(Cell* cell) {
	if (is_cell(cell)) cell->ref_count++;
	return cell;
}

Cell* detach(Cell* cell) {
	if (is_cell(cell)) {
		cell->ref_count--;
		if (cell->ref_count == 0) {
			cell_count--;
			detach(cell->head);
			detach(cell->tail);
		}
	}
	return cell;
}

Cell* alloc_gc() {
	for (int i=last_alloc+1; i<mem_size; i++) {
		if (mem[i].ref_count == 0) return &mem[i];
	}
	for (int i=0; i<last_alloc; i++) {
		if (mem[i].ref_count == 0) return &mem[i];
	}
	error_add("invalid alloc", 0);
	return prim.err;
}

Cell* cons(Cell* a, Cell* b) {
	if (a == prim.err || b == prim.err) return prim.err;
	Cell* new = alloc_gc();
	if (new == prim.err) return prim.err;
	last_alloc = new - mem; //oogly or compact?
	cell_count++;
	new->ref_count = 1;
	new->head = attach(a);
	new->tail = attach(b);
	return new;
}

Cell* cons_d(Cell* a, Cell* b) {
	Cell* new = cons(a, b);
	if (new == prim.err) return prim.err; //almost forgot TODO write test
	detach(a);
	detach(b);
	return new;
}

Cell* iden(Cell* a, Cell* b) { //TODO testing and error cell
	if (a == prim.err || b == prim.err) return prim.err;
	if (a == b) return prim.tru;
	return (a && b && iden(a->head, b->head) && iden(a->tail, b->tail))
		     ? prim.tru 
				 : 0;
}

Cell* head(Cell* cell) {
	if (cell == prim.err) return prim.err;
	if (!cell) {
		error_add("head - arg is an atom", 0);
		return prim.err;
	}
	return attach(cell->head);
}

Cell* tail(Cell* cell) {
	if (cell == prim.err) return prim.err;
	if (!cell) {
		error_add("tail - arg is an atom", 0);
		return prim.err;
	}
	return attach(cell->tail);
}

void write_list(FILE* f, Cell* cell) {
	int first = 1;
	while (cell) {
		if (!first) {
			fputc(' ', f);
		}
		write(f, cell->head);
		first = 0;
		cell = cell->tail;
	}
}

void write(FILE* f, Cell* cell) {
	if (cell == prim.err) fprintf(f, "ERROR");
	else if (cell == 0) fputc('~', f);
	else {
		fputc('[', f);
		write_list(f, cell);
		fputc(']', f);
	}
}

void write_nl(FILE* f, Cell* cell) {
	write(f, cell);
	fputc('\n', f);
}

Parse parse_list(char* str) {
	if (!str) {
		Parse p = {prim.err, 0};
		return p;
	}
	if (*str == 0) {
		error_add("parse_list - missing ']'", 0);
		Parse p = {prim.err, 0};
		return p;
	}
	if (*str == ' ') return parse_list(str + 1);
	if (*str == ']') {
		Parse p = {0, str + 1};
		return p;
	}
	Parse h = parse(str);
	Parse t = parse_list(h.rest);
	Parse p = {cons_d(h.cell, t.cell), t.rest};
	return p;
}

int is_number(char c) {
	if (c >= '0' && c <= '9') return 1;
	return 0;
}

int is_keyword(char c) {
	if ((c <= ' ')
			|| c > '~'
			|| c == '"'
			|| c == '['
			|| c == ']'
			|| is_number(c)) return 0;
	return 1;
}

int keyword_match(char* def, char* name) {
	int i = 0;
	for (; def[i]; i++) {
		if (def[i] != name[i]) return 0;
	}
	if (is_keyword(name[i]) || is_number(name[i])) return 0;
	return 1;
}

Parse parse_keyword(char* str) {
	char* next = str;
	while (is_keyword(*next) || is_number(*next)) next++; //change to dereferences
	for (int i=0; i<def_count; i++) {
		if (keyword_match(defs[i].name, str)) {
			Parse p = {attach(defs[i].cell), next};
			return p;
		}
	}
	error_add("parse_keyword - definition not found", 0);
	Parse p = {prim.err, 0};
	return p;
}


Parse parse(char* str) {
	/*if (!str) {
		Parse p = {prim.err, 0};
		return p;
	}*/ //TODO is it really unnecesary?
	if (*str == ' ') return parse(str + 1);
	if (*str == '[') return parse_list(str + 1);
	//if (*str == '"') return parse_string(str + 1);
	if (is_keyword(*str)) {
		return parse_keyword(str);
	}
	Parse p = {prim.err, 0}; //TODO error message
	return p;
}

Cell* read(char* str) {
	if (!str) return prim.err; 
	Parse p =  parse(str);
	if (p.cell == prim.err) return prim.err;
	if (*(p.rest) != 0) {
		error_add("read - trailing junk", 0);
		return prim.err;
	}
	return p.cell;
}

Def* def_add(char* name, Cell* cell) {//TODO errors
	Def* d = &defs[def_count];
	d->name = name;	
	d->cell = cell;	
	def_count++;
	return d;
}

void defs_init() {
	prim.subj = 0;
	def_add("~", prim.subj); //TODO chceck which tests will break with too many defs
	def_add("$", prim.subj); //TODO chceck which tests will break with too many defs
	def_add("F", prim.subj); //TODO remove??
	def_add("T", prim.tru);
	Cell* ctr;
	prim.iden = def_add("=", (ctr = prim.tru))->cell;
	prim.quot = def_add("'", (ctr = lus(ctr)))->cell;
	prim.head = def_add("<", (ctr = lus(ctr)))->cell;
	prim.tail = def_add(">", (ctr = lus(ctr)))->cell;
	prim.cons = def_add(":", (ctr = lus(ctr)))->cell;
	prim.eval = def_add("*", (ctr = lus(ctr)))->cell;
	prim.cond = def_add("?", (ctr = lus(ctr)))->cell;
	def_add("!", (ctr = lus(ctr)));
	detach(ctr);
}

Cell* lus(Cell* cell) {
	if (cell == prim.err) return prim.err;
	if (!cell) return cons(prim.tru, 0);
	Cell* h = head(cell);
	Cell* t = tail(cell);//should it attach??? TODO -- yes, tail is primitive access
	Cell* ret;
	if (h == prim.err || t == prim.err) ret = prim.err;
	else if (h) ret = cons_d(0, lus(t));//cons_d and the other cons
	else ret = cons(prim.tru, t); //TODO non-attaching prim.tru violates the rules, but won't crash, because it's static
	detach(h);
	detach(t);
	return ret;
}


//Cell* pock(Cell* subj, Cell* form) {return 0;}

Cell* pock(Cell* subj, Cell* form) {
	if (subj == prim.err || form == prim.err) return prim.err;
	if (!subj) {
		error_add("pock - no subject", 0);
		return prim.err;
	}
	if (!form) {
		error_add("pock - no formula", 0);
		return prim.err;
	}
	Cell* op = head(form); //attach?
	if (op == prim.err) {
		error_add("pock - formula is an atom", 0);
		return prim.err;
	}
	

	//is the use of cannonical tru good here? //TODO args, laziness, iden
	if (iden(op, prim.subj) == prim.tru) {
		detach(op);
		return attach(subj);//why does in decrease garbage?
	}

	

	Cell* form1 = tail(form);
	Cell* expr1 = head(form1);
	if (expr1 == prim.err) {
		error_add("pock - invalid first expression", 0);
		return prim.err;
	}

	if (iden(op, prim.quot) == prim.tru) {
		detach(op);//problem with detaching primitives nope?
		detach(form1);
		return expr1;
	} //expr1 and so on??? is that too much detaching?
	Cell* val1 = pock(subj, expr1);

	if (iden(op, prim.head) == prim.tru) {
		Cell* ret = head(val1);
		detach(op);
		detach(form1);
		detach(expr1);
		detach(val1);
		return ret;
	}
	if (iden(op, prim.tail) == prim.tru) {
		Cell* ret = tail(val1);
		detach(op);
		detach(form1);
		detach(expr1);
		detach(val1);
		return ret;
	}
	
	//why variadic is here and not at the end?
	//No special treatment for nth args
	if (iden(op, prim.iden) == prim.tru) {
		Cell* form_n = tail(form1);
		while(is_cell(form_n)) { //TODO with better error log you can give the exact erroneous arg
			Cell* expr_n = head(form_n);
			Cell* val_n = pock(subj, expr_n);
			Cell* res = iden(val1, val_n); //need not be detached, because iden returns cannonicals TODO define cannonicals
			Cell* form_n1 = tail(form_n);				 //^^ return value MUSN'T be detached you silly goose
			detach(expr_n);
			detach(val_n);
			detach(form_n);
			if(!is_cell(res)) {
				detach(form_n1);
				detach(op);
				detach(form1);
				detach(expr1);
				detach(val1);
				return res; //ponder the irregularity
			}
			form_n = form_n1;
		}
		detach(op);
		detach(form1);
		detach(expr1);
		detach(val1);
		detach(form_n);
		return prim.tru;
	}
	
	Cell* form2 = tail(form1);
	Cell* expr2 = head(form2); //expr2 and so on should be lazy
	if (expr2 == prim.err) {
		error_add("pock - invalid second expression", 0); //form2 can become garbage
		return prim.err;
	}

	if (iden(op, prim.cons) == prim.tru) {
		Cell* val2 = pock(subj, expr2);
		Cell* ret = cons_d(val1, val2);
		detach(op);
		detach(form1);
		detach(expr1);
		detach(form2);
		detach(expr2);
		return ret;
	}
	if (iden(op, prim.eval) == prim.tru) {
		Cell* val2 = pock(subj, expr2);
		Cell* ret = pock(val1, val2);
		detach(op);
		detach(form1);
		detach(expr1);
		detach(val1);
		detach(form2);
		detach(expr2);
		detach(val2);
		return ret;
	}
	if (iden(op, prim.cond) == prim.tru) {
		Cell* ret;
		Cell* form3 = tail(form2);
		Cell* expr3 = head(form3);
		if (expr3 == prim.err) { //ehh
			error_add("pock - invalid first expression", 0);
			return prim.err;
		}
		if (is_cell(val1)) {
			ret = pock(subj, expr2);
		} else {
			ret = pock(subj, expr3);
		}
		detach(op);
		detach(form1);
		detach(expr1);
		detach(val1);
		detach(form2);
		detach(expr2);
		detach(form3);
		detach(expr3);
		return ret;
	}
	
	error_add("pock - invalid op", 0);
	return prim.err;
}


int main() {
	tests();
	init(200, 100, 100);
	defs_init();
	
	error_print();
	close();
}
