#ifndef DECL_H
#define DECL_H



typedef struct Cell {
	int ref_count;
	struct Cell* head;
	struct Cell* tail;
} Cell;

typedef struct Error {
	int argc;
	char* name;
	Cell args[3];
} Error;

typedef struct Parse {
	Cell* cell;
	char* rest;
} Parse;

typedef struct Def {
	char* name;
	Cell* cell;
} Def;

typedef struct Primitives {
	Cell tru[1];
	Cell err[1];
	Cell* subj;
	Cell* quot;
	Cell*	iden;
	Cell* head;
	Cell* tail;
	Cell* cons;
	Cell* eval;
	Cell* cond;
} Primitives;

extern Primitives prim;

extern int cell_count;
extern int mem_size;
extern int last_alloc;
extern Cell* mem;


extern int max_error_count;
extern int error_count;
extern Error* errors;

extern int max_def_count;
extern int def_count;
extern Def* defs;


void init(int mem_size_, int max_error_count_, int max_def_count_);
void close();
void error_add(char* name, int argc);
void error_print();
int is_cell(Cell* cell);
int count_cells();
Cell* attach(Cell* cell);
Cell* detach(Cell* cell);
Cell* alloc_gc();
Cell* cons(Cell* a, Cell* b);
Cell* cons_d(Cell* a, Cell* b);
Cell* head(Cell* cell);
Cell* tail(Cell* cell);
Cell* iden(Cell* a, Cell* b);
void write_list(FILE* f, Cell* cell);
void write(FILE* f, Cell* cell);
void write_nl(FILE* f, Cell* cell);
int is_number(char c);
int is_keyword(char c);
int keyword_match(char* def, char* name);
Def* def_add(char* name, Cell* cell);
Parse parse_keyword(char* str);
Parse parse_list(char* str);
Parse parse(char* str);
Cell* read(char* str);
void defs_init();
Cell* lus(Cell* cell);
Cell* pock(Cell* subj, Cell* form);

void tests();



#endif

