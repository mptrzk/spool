#include <stdio.h>
#include <stdlib.h>
#include "eval.h"
#include "reader.h" //TODO do in a saner way
#include "dbg.h"

bool noun_equal(Noun* a, Noun* b) {
	if (a == b) return true;
	if (!a || !b) return false;
	if (noun_equal(a->head, b->head)
			&& noun_equal(a->tail, b->tail)) return true;
	return false;
}


Noun* pock_evlis(Noun* subj, Noun* list) {
	return list ? cons(pock_eval(subj, list->head),
										 pock_evlis(subj, list->tail)) : 0;
}


Noun* pock_eval(Noun* subj, Noun* form) {
	Noun* frame = mem.last;
	Noun* tcall = 0;
	Noun* tsubj = 0;
	Noun* tform = 0;
	while (1) {
		if (tcall) {
			subj = tcall->head;
			form = tcall->tail;
		}
		Noun* op = form->head; 
		Noun* args = form->tail; 
		if (dbg) {
			printf("\npock:\n");
			printf("subj:");
			noun_printnl(subj);
			printf("form:");
			noun_printnl(form);
			printf("op:");
			noun_printnl(op);
			printf("args:");
			noun_printnl(args);
			printf("frame: %ld\n", frame - mem.first);
		}
		if (!op->head) {
			if (noun_equal(op, prim.subj)) return gc(subj, frame);
			if (noun_equal(op, prim.quot)) return gc(args->head, frame);
			if (noun_equal(op, prim.head)) return gc(car(pock_eval(subj, args->head)), frame);
			if (noun_equal(op, prim.tail)) return gc(cdr(pock_eval(subj, args->head)), frame);
			if (noun_equal(op, prim.cons)) { 
				return gc(cons(pock_eval(subj, args->head),
												 pock_eval(subj, args->tail->head)), frame);
			}
			if (noun_equal(op, prim.list)) return gc(pock_evlis(subj, args), frame);
			//tail-recursive operators
			if (noun_equal(op, prim.eval)) {
				tsubj = pock_eval(subj, args->head);
				tform = pock_eval(subj, args->tail->head);
				tcall = gc(cons(tsubj, tform), frame); //redundant?
				continue;
			}
			if (noun_equal(op, prim.cond)) { 
				if (pock_eval(subj, args->head)) tform = args->tail->head;
				else tform = args->tail->tail->head;
				tcall = gc(cons(subj, tform), frame);
				continue;
			}
			if (noun_equal(op, prim.macr)) { 
				Noun* closure = pock_eval(subj, args->head);
				Noun* arglist = pock_evlis(subj, args->tail);
				tsubj = cons(arglist, closure->tail);
				tform = closure->head;
				tcall = gc(cons(tsubj, tform), frame);
				continue;
			}
			printf("Invalid op:\n");
			noun_printnl(op);
			exit(-1);
			return 0;
		}
		Noun* closure = pock_eval(subj, op);
		Noun* arglist = pock_evlis(subj, args);
		tsubj = cons(arglist, closure->tail);
		tform = closure->head;
		tcall = gc(cons(tsubj, tform), frame);
	} //TODO tcall as macro??
}

Noun* pock(char* subj_expr, char* form_expr) {
	return pock_eval(noun_read(subj_expr), noun_read(form_expr));
}
