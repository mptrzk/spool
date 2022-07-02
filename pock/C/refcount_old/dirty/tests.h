#pragma once

#include <string.h>

#include "all_declarations.h"


void tests() {
	/*eqv*/
	Cell* a = cons(0, cons(0, 0));
	assert(eqv(0, 0) || !"eqv - leaves");
	assert(eqv(a, a) || !"eqv - identity");
	assert(eqv(cons(0, 0), cons(0, 0)) || !"eqv - equivalence");
	assert(!eqv(0, cons(0, 0)) || !"eqv - leaf, cell");
	assert(!eqv(cons(0, 0), 0) || !"eqv - cell, leaf");
	assert(!eqv(cons(cons(0, 0), 0), cons(0, cons(0, 0))) || !"eqv - different heads, different tails");
	assert(!eqv(cons(0, 0), cons(0, cons(0, 0))) || !"eqv - same heads, different tails");
	assert(!eqv(cons(0, 0), cons(cons(0, 0), 0)) || !"eqv - same tails, different heads");

	/*is_single_char_token*/
	assert(is_single_char_token('['));
	assert(is_single_char_token('~'));
	//assert(is_single_char_token('!'));
	//assert(is_single_char_token('`'));
	assert(!is_single_char_token(0));
	assert(!is_single_char_token('0'));
	assert(!is_single_char_token('i'));
	assert(!is_single_char_token('A'));

	/*is_word_char*/
	assert(is_word_char('1'));
	assert(is_word_char('x'));
	assert(is_word_char('-'));
	assert(is_word_char('Z'));
	//assert(!is_word_char('!'));
	assert(!is_word_char('\n'));
	assert(!is_word_char(' '));
	assert(!is_word_char('~'));
	assert(!is_word_char(0));

	/*token_get*/
	Token t;
	t = token_get("");
	assert(t.len == 0 || !"token_get - empty string");
	t = token_get(" \n  ");
	assert(t.len == 0 || !"token_get - whitespace");
	t = token_get("foo-bar-5");
	assert(t.len == 9 || !"token_get - word");
	t = token_get("[");
	assert(t.len == 1 || !"token_get - single char token");
	t = token_get("\n \tfoo-bar-5   ");
	assert(t.len == 9 || !"token_get - word, trailing whitespace");
	t = token_get("foo-bar-5    ");
	assert(t.len == 9 || !"token_get - word, trailing whitespace");
	t = token_get("foo-bar-5[1]");
	assert(t.len == 9 || !"token_get - word, trailing tokens");
	t = token_get(" [foo[");
	assert(t.len == 1 || !"token_get - single char token, trailing tokens");
	t = token_get("   [");
	assert(t.len == 1 || !"token_get - single char token, leading whitespace");
	t = token_get("[   ");
	assert(t.len == 1 || !"token_get - single char token, trailing whitespace");
	t = token_get("\"foo bar\" bar");
	assert(t.len == 9 || !"token_get - string");

	/*tokenizer*/
	Token buf[10];
	memset(buf, 'a', sizeof(buf));
	tokenize(buf, "");
	assert(buf[0].len == 0 || !"tokenize - empty string");
	memset(buf, 'a', sizeof(buf));
	tokenize(buf, " \t  \n \r");
	assert(buf[0].len == 0 || !"tokenize - whitespace");
	memset(buf, 'a', sizeof(buf));
	tokenize(buf, "[");
	assert((buf[0].len == 1
				 	&& buf[1].len == 0)
				 || !"tokenize - STT");

	/*read*/
	eqv(read("~"), 0);
	eqv(read("[~]"), 0);
	eqv(read("[~ ~]"), cons(0, 0));
	eqv(read("[[~ ~] ~]"), cons(cons(0, 0), 0));
	eqv(read("[~ [~ ~]]"), cons(0, cons(0, 0)));
	eqv(read("[~ ~ ~]"), cons(0, cons(0, 0)));
	//cell_disp(read(""));
	//cell_disp(read("[]"));
	//cell_disp(read("[~ ~ ~] [~ ~]"));
	/*lus*/
	/*GC*/

	/*pock*/
	assert(eqv(pock(read("[[~ ~] ~]")), read("[~ ~]")));
	assert(eqv(pock(read("[[~ ~] ~ [~ ~ ~]]")), read("[~ ~ ~]")));
	assert(eqv(pock(read("[[[~ ~] [~ ~ ~]] [[~ ~] ~] ~]")), read("[~ ~]")));
	assert(eqv(pock(read("[[[~ ~] [~ ~ ~]] [~ [~ ~] ~] ~]")), read("[~ ~ ~]")));
	assert(eqv(pock(read("[[[~ ~] [~ ~ ~]] [[~ ~] ~] ~ [~ [~ ~] ~] [[~ ~] ~ ~]]")), read("[~ [~ ~] ~]")));
	assert(eqv(pock(read("[[[~ ~] [~ ~ ~]] [~ [~ ~] ~] ~ [~ [~ ~] ~] [[~ ~] ~ ~]]")), read("[[~ ~] ~ ~]")));
	assert(eqv(pock(read("[[[[~ ~ ~] [~ [~ ~] ~] ~] [~ ~ ~]] [[~ ~] [~ ~] ~] [[~ ~] ~] ~]")), read("[~ ~]")));
	assert(eqv(pock(read("[[[~ ~] ~ ~ ~] [~ ~ [~ ~] ~] [[~ [~ ~] ~] ~] [[~ ~] ~] ~]")), read("[[~ ~ ~] ~ ~]")));
	assert(eqv(pock(read("[[~ ~] [[~ ~] ~ [~ ~] ~] ~ [~ ~] [~ ~ ~]]")), read("~")));
	assert(eqv(pock(read("[~ [[~ ~] ~ [~ ~] ~] ~ [~ ~] [~ ~ ~]]")), read("[~ ~]")));
	//display(pock(read("[[~ ~] [[~ ~] ~]]"))); //seggie, add guards
	//display(pock(read("[~ [[~ ~] ~ [~ ~] ~] [[[~ ~] ~] ~] [~ ~] [~ ~ ~]]"))); //no seggie? should try fetch head of nil
	
	/*int_to_cell*/
	Cell* counter = 0;
	for (int i=0; i<20; i++) {
		assert(eqv(int_to_cell(i), counter));
		counter = lus(counter);
	}

	/*cell_to_int*/
	for (int i=0; i<20; i++) assert(i == cell_to_int(int_to_cell(i))); //TODO how many iterations is enough?

	printf("all tests passed\n");
}
