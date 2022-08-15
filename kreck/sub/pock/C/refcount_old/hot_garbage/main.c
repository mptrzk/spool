#include <stdio.h>
#include <stdlib.h>



typedef struct Cell {
	int ref_count;
	struct Cell* head;
	struct Cell* tail;
} Cell;


int mem_size = 0;
int last_alloc = 0;
Cell* mem;


void mem_init(int sz) {
	mem_size = sz;
	last_alloc = 0;
	mem = malloc(sz * sizeof(Cell));
	for (int i=0; i<sz; i++) mem[i].ref_count = 0;
}

void mem_free() {
	mem_size = 0;
	free(mem);
}

Cell* inc_ref(Cell* cell) {
	if (cell) cell->ref_count++;
	return cell;
}

Cell* dec_ref(Cell* cell) {
	if (cell) {
		cell->ref_count--;
		if (cell->ref_count == 0) {
		printf("foo\n");
			dec_ref(cell->head);
			dec_ref(cell->tail);
		}
	}
	return cell;
}

int get_free_idx() {
	for (int i=last_alloc+1; i<mem_size; i++) {
		if (mem[i].ref_count == 0) return i;
	}
	for (int i=0; i<last_alloc; i++) {
		if (mem[i].ref_count == 0) return i;
	}
	return -1; //last_alloc shenanigans
}

Cell* cons(Cell* a, Cell* b) {
	inc_ref(a);
	inc_ref(b);
	int idx = get_free_idx();
	if (idx == -1) {
		dec_ref(a);
		dec_ref(b);
		return 0;
	}
	last_alloc = idx;
	mem[idx].head = a;
	mem[idx].tail = b;
	return &mem[idx];
}

Cell* cons2(Cell* a, Cell* b) {
	int idx = get_free_idx(); //shouldn't inc_ref be done before? //pwn it
	if (idx == -1) return 0;
	last_alloc = idx;
	mem[idx].head = inc_ref(a);
	mem[idx].tail = inc_ref(b);
	return &mem[idx];
}

int main() {
	mem_init(2);
	printf("%p\n", mem);
	Cell* c = cons(0, cons(0, cons(0, 0)));
	//Cell* d = cons(0, 0);
	//printf("%p\n", d);
	mem_free();
	/*wololo*/
}
