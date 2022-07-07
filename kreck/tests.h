#include <assert.h>

void zerofill(int n) {
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
	Root* frame = rstack_top;
	Root* b = cons(a, a); //zerofill before it?
	assert(heap_sz() == 2);
	assert(rstack_sz() == 3);
	zerofill(3);
	assert(heap_sz() == 5);
	assert(rstack_sz() == 6);
	Root* fin = frame_return(frame, b);
	assert(rstack_sz() == 3); //frame 
	assert(heap_sz() == 5); //1st heap full
	zerofill(1);
	assert(heap_sz() == 3);
	assert(rstack_sz() == 4);
	zerofill(1);
	frame_return(frame, atom(0));
	zerofill(1);
	assert(heap_sz() == 3);
	assert(rstack_sz() == 4);
	grbfill(100);
	gc();
	assert(heap_sz() == 3);
	assert(rstack_sz() == 4); //tests using read/write?
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
	int sz = rstack_sz();
	Root* frame = rstack_top;
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

void tests() {
	test_init();
	test_gc();
	test_write();
	printf("all tests passed!\n");
}
