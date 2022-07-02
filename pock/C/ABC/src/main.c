#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>

#include "main.h"

int dbg;
int nodedup;
int cdbg;
int nogc;

void tests();


void init(int noun_cap, int def_cap) {
	mem_init(noun_cap);
	defs_init(def_cap);
}

void close() {
	free(mem.first);
	free(defs.first);
}


int main() {
	tests();
	init(1000, 100);	

	Noun* under = cons(0, cons(0, 0));
	Noun* frame = mem.last;
	Noun* garbage = cons(cons(cons(0, 0), 0), 0);
	Noun* a = cons(cons(0, 0), garbage);
	Noun* b = cons(under, cons(car(a), 0));
	printf("%ld\n", mem_count());
	printf("%ld\n", mem.last - frame);
	Noun* c = gc(b, frame);

	noun_read("[~ ~ ~ ~ ~ ~ ~ ~ ~]");
	printf("%ld\n", mem_count());
	printf("%ld\n", mem.last - frame);
	c = gc(c, frame);

	printf("%ld\n", mem_count());
	printf("%ld\n", mem.last - frame);
	noun_printnl(c);
	noun_printnl(noun_read("[~ [~ ~] ~]"));

	//noun_printnl(cons(0, cons(0, cons(0, 0))));
	//move back loop
	//set new last
	//
	//printf("goo\n");
	//
	Noun* f = 0;
	for (int i=0; i<10; i++) {
		noun_printnl(f);
		f = inc(f);
	}

	noun_printnl(add(noun_read("[[~]]"), noun_read("[[~] [~]]")));
	noun_printnl(mul10(0));

	gc(0, frame);
	Noun* bfn = noun_read("999999999999999999999999999999999999999999");
	printf("len %d\n", noun_len(bfn));
	noun_printnl(bfn);
	printf("%ld\n", mem_count());

	noun_printnl(def_get("!"));
	noun_printnl(noun_read("[~ ~ ~ ~]"));
	printf("%d\n", noun_equal(noun_read("10"), noun_read("[~ T ~ T]")));

	noun_printnl(pock("[T ~ T]", "[* [$] [' [> [$]]]]"));
	noun_printnl(pock("[T ~ T]", "[:: [$] [$] [$]]"));
	noun_printnl(pock("[T ~ T]", "[? [' ~] [$] [> [$]]]"));

	//noun_printnl(pock("[T T T]", "[[' [[: [< [$]] [< [$]]]]] [$] [$]]"));


	printf("zap!\n");
	//dbg = 1;
	//nodedup = 1;
	//cdbg = 1;
	//nogc = 1;
	noun_printnl(pock("[T T T]", "[[' [[: [< [$]] [< [$]]]]] [$]]"));
	//noun_printnl(pock("[T T T]", "[? [' T] [? [' T] [: [$] [$]] [$]] [$]]"));
	printf("de and\n");
	dbg = nogc = 0;
	printf("dsf\n");
	
	gc(0, frame); //WUT?
	printf("%ld\n", mem_count());
	close();
}
