#ifndef NOUN_H
#define NOUN_H


typedef struct Noun {
	struct Noun* head;
	struct Noun* tail;
} Noun;

typedef struct {
	int cap;
	Noun* first;
	Noun* last;
} Mem;

extern Mem mem;
extern Noun error[1];

long int mem_count();
Noun* cons(Noun* a, Noun* b);
Noun* car(Noun* noun);
Noun* cdr(Noun* noun);
Noun* noun_copy(Noun* noun, Noun* frame, Noun* copy_buf); 
Noun* gc(Noun* noun, Noun* frame);		
int noun_len(Noun* noun);
void mem_init(int noun_cap);

#endif
