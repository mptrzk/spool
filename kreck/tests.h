#include <assert.h>

void nilfill(int n) {
	for (int i=0; i<n; i++) atom(0);
}

void test_init() {
	init(5, 6);
	assert(heap_sz() == 0);
	assert(heap_cap == 5);
	assert(rstack_sz() == 0);
	assert(rstack_cap == 6);
	deinit();
}

//frame_return should pop any ptr in a stack
//how to test all meaningful cases?
//is it even possible?
//how about proving it?
//describing the frame_return based on what it does to the stack
//test frame_return separately?
//in a case where GC wouldn't collect?
//How are you sure it wouldn't collect
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
	printf("%ld\n", rstack_top - rstack_bot);
	printf("%ld\n", heap_top - heap_bot);
	frame_return(frame, atom(0));
	deinit();
}

void tests() {
	test_init();
	test_gc();
	printf("all tests passed!\n");
}
