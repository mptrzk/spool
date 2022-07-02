#include <stdio.h>
#include "writer.h"


void list_print(Noun* noun) {
	noun_print(car(noun));
	if (cdr(noun)) {
		printf(" ");
		list_print(cdr(noun));
	}
}

void cell_print(Noun* noun) {
	if (!noun) printf("cell: nil\n");
	else printf("cell: %ld\nhead: %ld\ntail: %ld\n\n",
						  noun - mem.first,
							noun->head - mem.first,
				      noun->tail - mem.first);
}

void noun_print(Noun* noun) {
	if (noun) {
		printf("[");
		list_print(noun);
		printf("]");
	}
	else printf("~");
}

void noun_printnl(Noun* noun) {
	noun_print(noun);
	printf("\n");
}
