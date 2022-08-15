#include <assert.h>
#include <string.h>
#include <stdio.h>
#include "decl.h"



int is_mem_empty() {
	for (int i=0; i<mem_size; i++) {
		if (mem[i].ref_count != 0) return 0;
	}
	return 1;
}

int count_cells() { //TODO is the above redundant?
	int count = 0;
	for (int i=0; i<mem_size; i++) {
		if (mem[i].ref_count != 0) count++;
	}
	return count;
}

int is_tree(Cell* cell) {
	if (cell == prim.err) return 0;
	if (cell == 0) return 1;
	if (cell->ref_count <= 0) return 0;
	return is_tree(cell->head) && is_tree(cell->tail);
}

void test_init() {
	init(100, 100, 100);
	assert(is_mem_empty());
	close();
}

void flush() {
	for (int i=0; i<mem_size; i++) {
		detach(cons(0, 0));
	}
}

void test_cons() { //TODO wrapping test
	init(3, 100, 100);
	Cell* a = cons(0, 0);
	assert(!is_mem_empty());
	Cell* b = cons(a, 0);
	Cell* c = cons(0, a);
	Cell* d = cons(0, c);
	assert(d == prim.err); //istrees
	assert(a->ref_count == 3);
	detach(a);
	assert(a->ref_count == 2);
	detach(b);
	assert(a->ref_count == 1);
	detach(c);
	assert(is_mem_empty());
	assert(error_count == 1);
	close();
}
	
void test_cons_d() {
	init(4, 100, 100);
	Cell* a = cons_d(cons_d(0, 0), cons_d(0, cons_d(0, 0)));
	Cell* b = cons_d(cons_d(a, a), 0);
	assert(b == prim.err);
	assert(!is_mem_empty()); //redundant?
	assert(error_count == 1);
	detach(a);
	assert(a->ref_count == 0);
	assert(is_mem_empty());
	close();
}

void test_iden() {
	init(20, 100, 100);
	assert(iden(0, 0));
	assert(!iden(cons(0, 0), 0));
	assert(!iden(0, cons(0, 0)));
	assert(iden(cons(cons(0, 0), 0), cons(cons(0, 0), 0)));
	assert(iden(cons(0, cons(0, 0)), cons(0, cons(0, 0))));
	close();
}

void test_head_tail() {
	init(10, 100, 100);
	Cell* a = cons(cons(0, 0), 0);
	Cell* b = cons(0, 0);
	Cell* c = cons(a, b);
	assert(iden(a, head(c)));
	assert(iden(b, tail(c)));
	assert(head(0) == prim.err);
	assert(tail(0) == prim.err);
	close();
}

int check_write(void (*fn)(FILE*, Cell*), Cell* cell, char* ref) {//should lambda
	FILE* f = tmpfile();
	fn(f, cell);
	fseek(f, 0, SEEK_END);
	int len = ftell(f) + 1;
	fseek(f, 0, SEEK_SET);
	char buf[len];
	fgets(buf, len, f);
	return !strcmp(ref, buf);
}

void test_write() {
	init(10, 100, 100);
	assert(check_write(write, 0, "~"));
	assert(check_write(write, cons_d(0, 0), "[~]"));
	assert(check_write(write, cons_d(0, cons_d(0, cons_d(0, 0))), "[~ ~ ~]"));
	assert(check_write(write, cons_d(0, cons_d(cons_d(cons_d(0, 0), 0), cons_d(0, 0))), "[~ [[~]] ~]"));
	assert(check_write(write, prim.err, "ERROR"));
	close();
}

int check_read(char* str) {
	return check_write(write, read(str), str);
}

void test_read() {
	init(1000, 10, 100);
	defs_init();
	//write_nl(stdout, read("~"));
	//printf("%s\n", defs[0].name);
	error_print();
	//TODO fix that mess
	assert(keyword_match("~", "~"));
	assert(check_read("~"));
	assert(keyword_match("~", "~]]"));
	//write_nl(stdout, read("[[~ [[~ ~] [~]] [~]] [~ ~]]"));
	error_print();
	assert(check_read("[[~ [[~ ~] [~]] [~]] [~ ~]]"));
	assert(iden(read("[[~ [~]] ~ ~ ]"), read("[[~ [~]] ~ ~]")) == prim.tru);
	assert(iden(read("[]"), read("~")) == prim.tru);
	assert(error_count == 0);
	assert(read("d") == prim.err);
	assert(read("[~ ~] d") == prim.err);
	close();
	init(1000, 10, 100);
	defs_init();
	Cell* a = read("[~ [~ [~] ~] ~ ~]");
	flush();
	assert(is_tree(a));
	detach(a);
	assert(!is_tree(a));
	//assert(is_mem_empty());
	assert(error_count == 0);
	close();
	init(5, 10, 100);
	defs_init();
	Cell* b = read("[~ ~ ~ ~ ~ ~]");
	assert(b == prim.err);
	close();
}

void test_char_predicates() {
	assert(is_number('1'));
	assert(!is_number('~'));
	assert(!is_number('@'));
	assert(!is_number('Z'));
	assert(!is_number('f'));
	assert(!is_number(' '));
	assert(!is_number('\n'));
	assert(is_keyword('K'));
	assert(is_keyword('k'));
	assert(is_keyword('!'));
	assert(!is_keyword(' '));
	assert(!is_keyword(']'));
	assert(!is_keyword('['));
	assert(!is_keyword('3'));
	assert(!is_keyword('\t'));
}

void test_lus() {
	init(40, 100, 100);
	defs_init();
	assert(iden(lus(read("[~]")), read("[T]")) == prim.tru);
	Cell* foo = iden(lus(read("[T]")), read("[F T]"));
	assert(foo == prim.tru);
	close();

	init(40, 100, 100);
	defs_init();
	Cell* a = read("[T]");
	int x = cell_count;
	Cell* b = attach(a);//!!! why did I make this error?
	//b = 0;
	for (int i=0; i<126; i++) {
		Cell* c = lus(b);
		//if (1) write_nl(stdout, b);
		assert(a->ref_count != 0);
		detach(b);
		b = c;
	}
	Cell* refr = read("[T T T T T T T]");
	error_print();
	assert(iden(b, refr) == prim.tru); //TODO iden tru??? change iden? is_cell?
	//assert(iden(b, read("[T T T T T T T]")) == prim.tru); // bug!
	detach(refr);
	detach(b);
	//write_nl(stdout, a);
	//printf("%d %d\n", cell_count, x);
	assert(cell_count == x);//TODO still
	close();
}

int check_pock(char* subj_code, char* form_code, char* ref_code) { //better name? return type?
	Cell* subj = read(subj_code);
	Cell* form = read(form_code);
	Cell* ref = read(ref_code);
	Cell* res = pock(subj, form);
	if (error_count) {
		write_nl(stdout, res);
		error_print();
	}
	int ret = is_cell(iden(ref, res));
	detach(subj);
	detach(form);
	detach(ref);
	detach(res);
	return ret;
}


void test_pock() { 
	init(100, 100, 100);	
	defs_init();


	int n = count_cells();


	assert(check_pock("[T T T]", "[$]", "[T T T]"));

	assert(check_pock("[T T T]", "[' ~]", "~"));
	assert(check_pock("[T T T]", "[' [~ T]]", "[~ T]"));

	assert(check_pock("[T T T]", "[< [$]]", "T"));

	assert(check_pock("[T T T]", "[> [$]]", "[T T]"));

	assert(check_pock("[T T T]", "[: [$] [' [F T]]]", "[[T T T] F T]"));

	//assert(check_pock("[T T T]", "[? [$] [' [F T]]]", "[F T]"));
	//TODO should it fail because of lack of 3rd expr?
	//yeah, it's not well formed
	
	assert(check_pock("[T T T]", "[? [$] [' [F T]] [> [$]]]", "[F T]"));
	assert(check_pock("[T T T]", "[? [< [< [$]]] [' [F T]] [> [$]]]", "[T T]"));

	assert(check_pock("[T T T]", "[= [$]]", "T"));
	assert(check_pock("[T T T]", "[= [$] [$]]", "T"));
	assert(check_pock("[T T T]", "[= [$] [< [$]]]", "F"));
	assert(check_pock("[T T T]", "[= [$] [' [T T T]] [: [< [$]] [' [T T]]] ]", "T"));
	assert(check_pock("[T T T]", "[= [$] [$] [$] [$] [$] [$] [$] [$]]", "T"));
	assert(check_pock("[T T T]", "[= [$] [$] [$] [$] [$] [< [$]] [$] [$]]", "F"));

	assert(count_cells() == n);
	close();
}

void tests() {
	test_init();
	test_cons();
	test_cons_d();
	test_iden();
	test_head_tail();
	test_write();
	test_read();
	test_char_predicates();
	test_lus();
	test_pock();
	printf("All tests passed\n");
}
