#include <assert.h>

void nilfill(int n) {
	for (int i=0; i<n; i++) atom(0);
}

void grbfill(int n) {
	Root* frame = rstack_top;
	for (int i=0; i<n; i++) {
		atom(0);
		rstack_top = frame;
	}
}

void test_init() {
	init(5, 6);
	assert(heap_sz() == 0);
	assert(heap_cap == 5);
	assert(rstack_sz() == 0);
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
*/
void test_gc() {
	init(5, 100);
	Root* a = atom(10);
	Root* frame = rstack_top;
	Root* b = cons(a, a); //nilfill before it?
	assert(heap_sz() == 2);
	assert(rstack_sz() == 2);
	nilfill(3);
	assert(heap_sz() == 5);
	assert(rstack_sz() == 5);
	Root* fin = frame_return(frame, b);
	assert(rstack_sz() == 2); //frame 
	assert(heap_sz() == 5); //1st heap full
	nilfill(1);
	assert(heap_sz() == 3);
	assert(rstack_sz() == 3);
	nilfill(1);
	frame_return(frame, atom(0));
	nilfill(1);
	printf("%ld\n", heap_sz()); //TODO 'xplain
	assert(heap_sz() == 3);
	assert(rstack_sz() == 3);
	grbfill(100);
	assert(rstack_sz() == 3); //tests using read/write?
	deinit();
}


/*
testing read and write together?
verifying that GC has no effect?
 shouldn't it be done by GC test?
*/
void test_write() {
	
}

void tests() {
	test_init();
	test_gc();
	test_write();
	printf("all tests passed!\n");
}
