#include <string.h>

void test_init_heap() {
	init(10);
	assert(heap.cap == 10);
	assert(heap.top == heap.bot);
	assert(heap_sz() == 0);
	deinit();
}

void test_cons() {
	init(10);
	Noun* a = cons(0, 0);
	assert(heap_sz() == 1);
	assert(a->car == 0);
	assert(a->cdr == 0);
	deinit();
}

void test_word() {
	init(10);
	Noun* a = word(1337);
	assert(heap_sz() == 1);
	assert(a->car == (Noun*) 1337);
	assert(a->cdr == word_sen);
	deinit();
}

void test_iden() {
	init(10);
	Noun* a = word(1337);
	Noun* b = cons(a, a);
	assert(iden(0, 0));
	assert(iden(a, a));
	assert(iden(b, b));
	assert(iden(a, word(1337)));
	assert(iden(word(1337), a));
	assert(!iden(a, b));
	assert(!iden(a, 0));
	assert(!iden(b, 0));
	assert(!iden(b, word(2)));
	deinit();
}

/*
number of copied noun = number of unique nouns above the frame,
reached by the function call
the function should be called on a tree that:
	contains words, nils, nouns
	contains nouns from below the frame
	contains non-unique nouns from above the frame

*/
void test_noun_copy() {
	init(10);
	Noun* a = cons(word(1337), 0); //stuff below the frame
	Noun* frame = make_frame();
	assert(frame - heap.bot == 2);
	Noun* b = cons(word(5), 0);
	Noun* c = cons(a, cons(b, b));
	Noun* copy_buf = heap.top;
	noun_copy(c, frame, copy_buf);
	assert(heap.top - copy_buf == 4);
	deinit();
} 

//testing correctness is done in GC test
//noun_copy returns addresses of future nouns
//copied nouns are invalid until shifted
//gc body does that
//repeat the same tests, but with identity check?
void test_gc() {
	init(50);
	Noun* a = cons(word(1337), 0);
	Noun* frame = make_frame();
	Noun* b = cons(word(5), 0);
	Noun* c = cons(a, cons(b, b)); //stuff below
	cons(word(1), word(2)); //some garbage
	Noun* ret = gc(c, frame);
	assert(heap.top - frame == 4);
	assert(heap_sz() == 6);
	assert(iden(c, cons(cons(word(1337), 0), cons(cons(word(5), 0), cons(word(5), 0)))));
	assert(iden(ret, cons(cons(word(1337), 0), cons(cons(word(5), 0), cons(word(5), 0)))));
	//gc(0, frame);
	//assert(frame - heap.bot == 2);
	deinit();
}

int check_write(Noun* noun, char* ref) {
	FILE* f = tmpfile();
	noun_write(noun, f);
	fseek(f, 0, SEEK_END);
	int len = ftell(f) + 1;
	fseek(f, 0, SEEK_SET);
	char buf[len];
	fgets(buf, len, f);
	fclose(f);
	return !strcmp(ref, buf);
}

void test_write() {
	init(50);
	assert(check_write(0, "()")); //TODO noinit flag?
	assert(check_write(word(1337), "1337"));
	assert(check_write(cons(0, word(1337)), "(() . 1337)"));
	assert(check_write(cons(word(0), word(1337)), "(0 . 1337)"));
	assert(check_write(cons(word(0), word(1337)), "(0 . 1337)"));
	assert(check_write(cons(word(1337), 0), "(1337)"));
	assert(check_write(cons(word(1337), cons(0, 0)), "(1337 ())"));
	assert(check_write(cons(cons(word(1337), 0), cons(0, 0)), "((1337) ())"));
	deinit();
}

int check_read_write(char* str) {
	return check_write(rread(str), str);
}

void test_read() {
	init(50);
	assert(check_read_write("()"));
	assert(check_read_write("5"));
	assert(check_read_write("(1 . 2)"));
	assert(check_read_write("(1 (2 3) 4)"));
	assert(check_read_write("((1 1) (2 3) 4 . 5)"));
	deinit();
}

//TODO paste and revise word_equal test

void test_defs() {
	init(50);
	assert(deflist == 0);
	assert(iden(def_get("leet"), 0));
	def_add("leet", word(1336)); 
	assert(iden(def_get("leet"), word(1336)));
	def_add("foo", word(33)); 
	assert(iden(def_get("leet"), word(1336)));
	def_add("leet", word(1337)); 
	assert(iden(def_get("leet"), word(1337)));
	def_add("nil", 0);
	assert(iden(def_get("nil"), 0));
	deinit();
	//TODO explain how the first test failed because of string constants
	//having different addresses
}

void test_read_defs() {
	init(50);
	def_add("leet", word(1337));
	assert(iden(rread("leet"), rread("1337")));
	assert(iden(rread("(leet)"), rread("(1337)")));
	assert(iden(rread("(leet . 0)"), rread("(1337 . 0)")));
	def_add("leet", rread("(leet leet)")); //pathological?
	assert(iden(rread("leet"), rread("(1337 1337)"))); 
	def_add("foo", rread("(leet)"));
	assert(iden(rread("(foo)"), rread("(((1337 1337)))")));
	def_add("nil", 0);
	assert(iden(rread("nil"), rread("()")));
	deinit();
}

void test_kreck() {
	//init(1000); why does this break the heap? shouldn't assertion fail first?
	init(2000);
	init_defs();
	printf("%ld\n", heap_sz());
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
	writenl(kreck("(1 2 3)", "((! gate () (map (! gate () (: arg1 arg1) ) arg3)) ($) (< ($)) (> ($)))"));
	//writenl(kreck("(1 2 3)", "((! gate () (rec1)))"));
	//TODO empty exprs
	//TODO is it really tail recursive?
	//it is, because it can run the infinite loop below:
	dbg = 1;
	writenl(kreck("()", "((' (((:: (< (> ($))) (< (> ($))))) ((:: (< (> ($))) (< (> ($))))))) )" ));
	deinit();
}

void tests() {
	test_init_heap();
	test_cons();
	test_iden();
	test_noun_copy();
	test_gc();
	test_write();
	test_read();
	test_defs();
	test_read_defs();
	test_kreck();
	printf("All tests passed!\n");
}
