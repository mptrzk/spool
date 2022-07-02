#ifndef EVAL_H
#define EVAL_H

#include <stdbool.h>
#include "noun.h"

bool noun_equal(Noun* a, Noun* b);
Noun* pock_eval(Noun* subj, Noun* form);
Noun* pock_evlis(Noun* subj, Noun* list);
Noun* pock_eval(Noun* subj, Noun* form);
Noun* pock(char* subj_expr, char* form_expr);

#endif
