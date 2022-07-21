#include <assert.h>

void zerofill(int n) {
	for (int i=0; i<n; i++) atom(0);
}

void grbfill(int n) {
	Root* frame = make_frame();
	for (int i=0; i<n; i++) {
		atom(0);
		frame_restore(frame);
	}
}

void test_init() {
	init(5, 6);
	assert(heap_sz() == 0);
	assert(heap_cap == 5);
	assert(rstack_sz() == 1);
	assert(rstack_cap == 6);
	deinit();
}

/*
frame_return should pop any ptr in a stack
how to test all meaningful cases?
is it even possible?
how about proving it?
describing the frame_return based on what it does to the stack
test frame_return separately?
in a case where GC wouldn't collect?
How are you sure it wouldn't collect

testing function vs testing behavior
*/

/*
	that test seems rigid
*/
void test_gc() {
	init(5, 100);
	Root* a = atom(10);
	Root* frame = make_frame();
	Root* b = cons(a, a); //zerofill before it?
	assert(heap_sz() == 2);
	assert(rstack_sz() == 3);
	zerofill(3);
	assert(heap_sz() == 5);
	assert(rstack_sz() == 6);
	frame_return(frame, b);
	assert(rstack_sz() == 3); //frame 
	assert(heap_sz() == 5); //1st heap full
	zerofill(1);
	assert(heap_sz() == 3);
	assert(rstack_sz() == 4);
	frame_return(frame, cons(atom(1), nil()));
	zerofill(1);
	assert(heap_sz() == 4);
	assert(rstack_sz() == 4);
	grbfill(100);
	gc();
	assert(heap_sz() == 4);
	assert(rstack_sz() == 4); //tests using read/write?
	deinit();
	init(100, 100);
	a = rread("(3 (1))");
	gc();
	assert(iden(a, rread("(3 (1))")));
	gc();
	assert(iden(a, rread("(3 (1))")));
	deinit();
}


/*
testing read and write together?
verifying that GC has no effect?
 shouldn't it be done by GC test?
*/

/*
 * testing for rstack leaks
*/

int check_write(Cell* cell, char* ref) {
	FILE* f = tmpfile();
	cell_write(cell, f);
	fseek(f, 0, SEEK_END);
	int len = ftell(f) + 1;
	fseek(f, 0, SEEK_SET);
	char buf[len];
	fgets(buf, len, f);
	fclose(f);
	return !strcmp(ref, buf);
}

int check_read_write(char* str) {
	return check_write(*rread(str), str);
}

void test_write() {
	init(100, 100);
	assert(check_write(*nil(), "()"));	
	assert(check_write(*atom(0), "0"));	
	assert(check_write(*cons(atom(11), atom(0)), "(11 . 0)"));	
	assert(check_write(*cons(atom(11), nil()), "(11)"));	
	//writenl(rread("(0 122 . 0)"));
	
	assert(check_read_write("(1 (2 3) 4)"));
	assert(check_read_write("(1 . 2)"));
	assert(check_read_write("((1 1) (2 3) 4 . 5)"));
	deinit();
	//TODO error cases
}

void test_word_equal() {
	assert(word_equal("abc", "abc"));
	assert(!word_equal("abcd", "abc"));
	assert(!word_equal("abc", "abcd"));
	assert(word_equal("abc", "abc d"));
	assert(word_equal("abc d", "abc"));
	assert(!word_equal("abc d", "abcd"));
	assert(!word_equal("abcd", "abc d"));
	assert(word_equal("abc\nd", "abc d"));
}

//testing iden before I/O stuff?
void test_iden() {
	init(100, 100);
	Root* a = rread("(1 2 3)");
	assert(iden(nil(), nil()));
	assert(iden(a, a));
	assert(iden(a, rread("(1 2 3)")));
	assert(iden(rread("(1 2 3)"), a));
	assert(!iden(nil(), atom(0)));
	assert(!iden(atom(99), atom(0)));
	deinit();
}

void test_car_cdr() {
	init(100, 100);
	Root* a = rread("(1 2 3)");
	assert(iden(car(a), rread("1")));
	assert(iden(car(rread("()")), nil()));
	assert(iden(cdr(a), rread("(2 3)")));
	assert(iden(cdr(rread("(1)")), nil()));
	assert(iden(cdr(rread("()")), nil()));
	assert(!iden(rread("1"), rread("(2 3)")));
	assert(!iden(car(a), cdr(a)));
	deinit();
}

void test_defs() {
	init(100, 100);
	assert(!(*def_get("foo")));
	def_add("foo", rread("10"));
	def_add("~", nil());
	assert(iden(def_get("foo"), rread("10")));
	assert(iden(def_get("~"), nil()));
	def_add("foo", rread("(1 2 3)"));
	def_add("bar", atom(20));
	assert(iden(def_get("foo"), rread("(1 2 3)")));
	deinit();
}

//TODO some tests on the root stack

void test_rread() {
	init(100, 100);
	def_add("foo", rread("(1 2 3)"));
	def_add("bar", rread("(foo . foo)"));
	assert(iden(rread("(0 (1 2 3) 4)"), rread("(0 foo 4)")));
	assert(iden(rread("bar"), rread("((1 2 3) 1 2 3)")));
	deinit();
}

//TODO fix reading nil
void test_kreck() {
	init(400, 100);
	init_defs();
	kreck("(1 2 3)", "(: (> (> ($))) (< ($)))");
	assert(iden(kreck("(1 2 3)", "($)"), rread("(1 2 3)")));
	assert(iden(kreck("(1 2 3)", "(' (4 5 6))"), rread("(4 5 6)")));
	assert(iden(kreck("(1 2 3)", "(< ($))"), rread("1")));
	assert(iden(kreck("(1 2 3)", "(> ($))"), rread("(2 3)")));
	assert(iden(kreck("(1 2 3)", "(: ($) ($))"), rread("((1 2 3) 1 2 3)")));
	assert(iden(kreck("(1 2 3)", "(: (> (> ($))) (< ($)))"), rread("((3) . 1)")));
	assert(iden(kreck("(1 2 3)", "(: (> (> ($))) (< ($)))"), rread("((3) . 1)")));
	assert(iden(kreck("(1 2 3)", "(* (' (4 5 6)) (' (< (> ($)))))"), rread("5")));
	assert(iden(kreck("((1 2 3) (< (> ($))))", "(* (< ($)) (< (> ($))))"), rread("2")));
	assert(iden(kreck("(1 2 3)", "(? (' 7) (< ($)) (> ($)))"), rread("1")));
	assert(iden(kreck("(1 2 3)", "(? (' ()) (< ($)) (> ($)))"), rread("(2 3)")));
	assert(iden(kreck("(1 2 3)", "(:: ($) (< ($)) (> ($)) (' 4))"), rread("((1 2 3) 1 (2 3) 4)"))); 
	assert(iden(kreck("(1 2 3)", "((' (($))) (< ($)) (> ($)))"), rread("((1 (2 3)))")));
	printf("reached\n");
	writenl(kreck("()", "((' (((:: (< (> ($))) (< (> ($))))) ((:: (< (> ($))) (< (> ($))))))) )" ));
	deinit();
}

void tests() {
	test_init();
	//test_gc();
	test_write(); //TODO it kinda tests read too, sort that out
	test_word_equal();
	test_iden(); //TODO why didn't I write tests thorough enough the first time?
	test_car_cdr();
	test_defs();
	test_rread(); //TODO tests that would catch the error with mutated deflist
	test_kreck();
	printf("all tests passed!\n");
}
