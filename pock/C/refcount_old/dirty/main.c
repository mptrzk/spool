#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include "sources.h"



typedef struct Cell {
	int ref_count;
	struct Cell* head;
	struct Cell* tail;
} Cell;


typedef struct {
	char* str;
	int len;
} Token;


struct {
	/* 0th order */
	Cell* troo;
	Cell* quot;
	Cell* head;
	Cell* tail;
	Cell* eval;
	Cell* cons;
	Cell* cond;
	Cell* macro;
	/* 1st order */
	Cell* data;
	Cell* code;
	Cell* call;
	Cell* make_gate;
	/* 2nd order */
	Cell* lus;
	Cell* eqv;
	/* 3rd order */
	Cell* expand;
} cst;

typedef struct {
	char* name;
	Cell* value;
} Macro;

int cell_max;
int cell_count;
Cell* mem;
Cell* alloc_ptr;
Cell* last_cell;
int fudge;



//TODO deprecate?
Cell* head(Cell* cell) {
	if (cell) return cell->head;
	return 0;
}

Cell* tail(Cell* cell) {
	if (cell) return cell->tail;
	return 0;
}

Cell* inc_ref(Cell* cell) {
	if (cell) {
		cell->ref_count++;
		cell_count++;
	}
	return cell;
}

Cell* dec_ref(Cell* cell) {
	if (cell) {
		cell->ref_count--;
		cell_count--;
	}
	return cell;
}

int find_next_alloc() {
	for (Cell* ptr = last_cell + 1; ptr - mem < cell_max; ptr++) {
		if (ptr->ref_count == 0) {
			dec_ref(ptr->head);
			dec_ref(ptr->tail);
			alloc_ptr = ptr;
			return 1;
		}
	}
	for (Cell* ptr = mem; ptr != last_cell; ptr++) {
		if (ptr->ref_count == 0) {
			alloc_ptr = ptr;
			dec_ref(ptr->head);
			dec_ref(ptr->tail);
			return 1;
		}
	}
	return 0;
}

Cell* cons(Cell* h, Cell* t) { //TODO skipping taken cells
	last_cell = alloc_ptr;
	last_cell->ref_count = 0;
	last_cell->head = inc_ref(h);
	last_cell->tail = inc_ref(t);
	assert(find_next_alloc() || !"out of memory");
	return last_cell;
}

int eqv(Cell* a, Cell* b) {
	if (a == b) return 1;
	if (a && b && eqv(a->head, b->head) && eqv(a->tail, b->tail)) return 1;
	//if (a && b && eqv(a->head, b->head) && eqv(b->tail, b->tail)) return 1;  // your tests SUCK :)
	return 0;
}

void cell_print(Cell* cell, int elide) {
	if (cell) {
		if (!elide) putchar('[');
		cell_print(cell->head, 0);
		putchar(' ');
		cell_print(cell->tail, (cell->tail) ? 1 : 0);
		if (!elide) putchar(']');
	} else putchar('~');
}

void display(Cell* cell) {
	cell_print(cell, 0);
	putchar('\n');
}

int is_word_char(char c) {
	if ((c >= '0' && c <= '9')
			|| (c >= 'a' && c <= 'z')
			|| (c >= 'A' && c <= 'Z')) return 1;
	char* specials = "-!@#$%^&:;,',<>*.?";
	for (int i=0; specials[i]; i++) {
		if (c == specials[i]) return 1;
	}
	return 0;
}


int is_single_char_token(char c) { //single char tokens
	char stts[] = "~[]";
	for (int i=0; stts[i]; i++) {
		if (c == stts[i]) return 1;
	}
	return 0;
}

Token token_get(char* str) {
	if (*str == 0) {
		Token tok = {0, 0};
		return tok;
	}
	if (*str == '"') {
		int i = 1;
		for (; str[i] != '"'; i++);
		Token tok = {str, i+1};
		return tok;
	}
	if (is_word_char(*str)) {
		int len = 0;
		for (; is_word_char(str[len]); len++);
		Token tok = {str, len};
		return tok;
	}
	if (is_single_char_token(*str)) {
		Token tok = {str, 1};
		return tok;
	}
	return token_get(str + 1);
}

void tokenize(Token* token_ptr, char* str) {
	do {
		*token_ptr = token_get(str);
		str = token_ptr->str + token_ptr->len;
		token_ptr++;
	} while (token_ptr[-1].len != 0);
}


void token_print(Token* token) {
	if (token->str) {
		for (int i=0; i<token->len; i++) putchar(token->str[i]);
		putchar('\n');
	} else printf("nil\n");
}

int is_terminator(Token* token) {
	return (!token->str || token->str[0] == ']') ? 1 : 0;
}

Token* subexp_next(Token* token) {
	if (is_terminator(token)) return token;
	int bracket_count = 0;
	do {
		if (token->str[0] == '[') bracket_count++;
		if (token->str[0] == ']') bracket_count--;
		token++;
	} while (bracket_count);
	return token;
}

int token_string_cmp(Token* tok, char* str) {
	for (int i=0; i<tok->len; i++) {
		if (tok->str[i] != str[i]) return 0;
	}
	if (str[tok->len] != 0) return 0;
	return 1;
}

Cell* read(char* expr);
Cell* macro_get(Token* token) {
	/* 0th order */
	if (token_string_cmp(token, "'")) return cst.quot;
	if (token_string_cmp(token, "<")) return cst.head;
	if (token_string_cmp(token, ">")) return cst.tail;
	if (token_string_cmp(token, "*")) return cst.eval;
	if (token_string_cmp(token, ".")) return cst.cons;
	if (token_string_cmp(token, "?")) return cst.cond;
	if (token_string_cmp(token, "!")) return cst.macro;
	/* 1st order */
	if (token_string_cmp(token, "#data")) return cst.data;
	if (token_string_cmp(token, "#code")) return cst.code;
	if (token_string_cmp(token, "#call")) return cst.call;
	if (token_string_cmp(token, "#make-gate")) return cst.make_gate;
	/* 2nd order */
	if (token_string_cmp(token, "#lus")) return cst.lus;
	if (token_string_cmp(token, "#eqv")) return cst.eqv;
	/* 3rd order */
	if (token_string_cmp(token, "#expand")) return cst.expand;
	return 0;
}

Cell* parse_list(Token* tokens);
Cell* parse(Token* tokens) { //TODO empty string
	assert(!is_terminator(tokens)); //would this thing need to also be in parse_list, if arg eval order didn't matter?
	//if (fudge) token_print(tokens); //ponder why the tail printed first
	if (tokens->str[0] == '[') return parse_list(tokens + 1);
	return macro_get(tokens);
}

Cell* parse_list(Token* tokens) {
	Token* list_tail = subexp_next(tokens);
	if (is_terminator(list_tail)) return parse(tokens);
	return cons(parse(tokens), parse_list(list_tail));
}

Cell* read(char* expr) {
	Token token_buf[1000];
	tokenize(token_buf, expr);
	assert(!subexp_next(token_buf)->str || !"unexpected tokens after the expression");
	return parse(token_buf);
}

Cell* lus(Cell* n) {
	if (eqv(n, read("~"))) return read("[[~ ~] ~]");
	if (eqv(head(n), read("~"))) return cons(read("[~ ~]"), tail(n));
	if (eqv(head(n), read("[~ ~]"))) return cons(read("~"), lus(tail(n)));
	return 0;
}

Cell* pock(Cell* noun) {
	Cell* subj = noun->head;
	Cell* form = noun->tail;
	//~ and head?
	if (!form) return subj;
	Cell* op = form->head;
	Cell* rest = form->tail;
	if (eqv(op, cst.quot)) return rest;
	if (eqv(op, cst.head)) return pock(cons(subj, rest))->head;
	if (eqv(op, cst.tail)) return pock(cons(subj, rest))->tail;
	if (eqv(op, cst.eval)) return pock(pock(cons(subj, rest)));
	if (eqv(op, cst.cons)) return cons(pock(cons(subj, rest->head)),
																							 pock(cons(subj, rest->tail)));
	//if (eqv(op, read("[[~ ~] ~ [~ ~] ~"))) { //reader seggie
	if (eqv(op, cst.cond)) {
		if (pock(cons(subj, rest->head))) return pock(cons(subj, rest->tail->head));
		return pock(cons(subj, rest->tail->tail));
	}
	display(noun);
	assert(!"pock - invalid operation");
	return 0;
}

Cell* int_to_cell(int n) {
	if (n) return cons((n % 2) ? cst.troo : 0, int_to_cell(n / 2));
	return 0;
}

/* //TODO ponder why it is wrong
int cell_to_int(Cell* cell) {
	int n = 0;
	for (; cell; cell=cell->tail) {
		n *= 2;
		if (cell->head) n += 1;
	}
	return n;
}
*/

int cell_to_int(Cell* cell) {
	int n = 0;
	int bitval = 1;
	for (; cell; cell=cell->tail) {
		if (cell->head) n += bitval;
		bitval *= 2;
	}
	return n;
}


// TODO cst.eval = inc_ref(read("[[~ ~] [~ ~] ~")); //seggie, no terminator
void init_cst() {
	/* 0th order */
	cst.troo = inc_ref(read("[~ ~]"));
	cst.quot = 0;
	cst.head = inc_ref(read("[[~ ~] ~]"));
	cst.tail = inc_ref(read("[~ [~ ~] ~]"));
	cst.eval = inc_ref(read("[[~ ~] [~ ~] ~]"));
	cst.cons = inc_ref(read("[~ ~ [~ ~] ~]"));
	cst.cond = inc_ref(read("[[~ ~] ~ [~ ~] ~]"));
	cst.macro = inc_ref(read("[~ [~ ~] [~ ~] ~]"));
	/* 1st order */
	cst.data = inc_ref(read("[< ~]"));
	cst.code = inc_ref(read("[> ~]"));
	cst.call = inc_ref(read("[* . ~ > ~]"));
	cst.make_gate = inc_ref(read(src_make_gate));
	/* 2nd order */
	cst.lus = inc_ref(read(src_lus));
	cst.eqv = inc_ref(read(src_eqv));
	/* 3rd order */
	cst.expand = inc_ref(read(src_expand));

}

void init() {
	cell_max = 3000;
	mem = malloc(cell_max * sizeof(Cell));
	for (int i=0; i<cell_max; i++) { //is there a cleaner way?
		mem[i].ref_count = 0;
		mem[i].head = 0;
		mem[i].tail = 0;
	}
	alloc_ptr = mem;
	last_cell = mem - 1;//is that voodoo?
	init_cst();
}


#include "tests.h"


int main() {
	fudge = 0;
	init();
	tests();
	//display(pock(read("[[[[~ ~] [~ ~] [~ ~] ~] #lus] #call]")));
	//display(pock(read("[[[[~ ~ ! ~] #lus] #expand] #call]")));
	//display(pock(read("[[[[~ ~ ! [~ ~] [~ ~] [~ ~] ~] #lus] #expand] #expand]")));
	display(pock(read("[* . [' ~ ~] * . [' ? ~ [' ~] [' ~ ~]] ' #make-gate]")));
	free(mem);
}
