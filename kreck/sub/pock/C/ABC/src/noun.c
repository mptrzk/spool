#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <assert.h>
#include "noun.h"
#include "dbg.h"

Mem mem;
Noun error[1];

long int mem_count() {
	return mem.last - mem.first;
}

Noun* car(Noun* noun) {
	return noun->head;
}

Noun* cdr(Noun* noun) {
	return noun->tail;
}


Noun* cons(Noun* a, Noun* b) {
	Noun* ret = mem.last;
	ret->head = a;	
	ret->tail = b;	
	mem.last++;
	assert(mem_count() <= mem.cap);
	return ret; 
}


Noun* noun_copy(Noun* noun, Noun* frame, Noun* copy_buf) { 
	static Noun copied[1];
	if (!noun) return 0;
	if (noun - mem.first < frame - mem.first) return noun; //is this ptr arithmetic necessary?
	if (!nodedup) if (noun->head == copied) return noun->tail; //checking if noun was copied
	int offset = copy_buf - frame;
	Noun* ret = cons(noun_copy(noun->head, frame, copy_buf),
									 noun_copy(noun->tail, frame, copy_buf)) - offset; 
	if (!nodedup) noun->head = copied;
	if (!nodedup) noun->tail = ret;
	return ret;
}

Noun* gc(Noun* noun, Noun* frame) {		
	if (nogc) return noun;
	Noun* copy_buf = mem.last;
	int cdbg_old = cdbg;
	cdbg = 0;
	noun_copy(noun, frame, copy_buf);
	cdbg = cdbg_old;
	int n = mem.last - copy_buf;
	if (!n) {
		mem.last = frame;
		return noun;
	}
	for (int i=0; i<n; i++) {
		frame[i] = copy_buf[i];
	}
	mem.last = frame + n;
	return mem.last - 1;
}


int noun_len(Noun* noun) { //move?
	int i;
	for (i=0; noun; noun=noun->tail) i++;
	return i;
}

void mem_init(int noun_cap) {
	mem.cap = noun_cap;
	mem.first = malloc(noun_cap * sizeof(Noun));
	mem.last = mem.first;
}

/*
Noun* cons(Noun* a, Noun* b) {
	assert(a < mem.last);
	assert(b < mem.last);
	if (cdbg) {
		printf("\ncons:\n");
		printf("a: ");
		noun_printnl(a);
		printf("b: ");
		noun_printnl(b);
	}
	Noun* ret = mem.last;
	ret->head = a;	
	ret->tail = b;	
	mem.last++;
	assert(mem_count() <= mem.cap);
	return ret; 
}
*/

/*
Noun* gc(Noun* noun, Noun* frame) {		
	if (nogc) return noun;
	if (dbg) {
	printf("gc:\nnoun: %ld\nframe: %ld\n last: %ld\n\n",
				 noun - mem.first,
				 frame - mem.first,
				 mem_count());
	}
	Noun* copy_buf = mem.last;
	//noun_printnl(noun);
	int cdbg_old = cdbg;
	cdbg = 0;
	noun_copy(noun, frame, copy_buf);
	cdbg = cdbg_old;
	//noun_printnl(res);
	int n = mem.last - copy_buf;
	//printf("frame: %ld\n", frame - mem.first);
	//printf("copy_buf: %ld\n", copy_buf - mem.first);
	//printf("n: %d\n", n);
	if (!n) {
		mem.last = frame;
		return noun;
	}
	for (int i=0; i<n; i++) {
		frame[i] = copy_buf[i]; //noun children pointers need to be shifted aswell
	}
	mem.last = frame + n;
	//noun_printnl(mem.last - 1);
	return mem.last - 1;
}
*/
