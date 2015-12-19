#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include "sen_linked_list.h"
#include "sen_linked_list.h"

#ifndef SEN_INIT_BASE_GRPS
#define SEN_INIT_BASE_GRPS

struct snt_Board {
    Sen_list cells;
} b;

struct snt_Piece {
    int snt_owner;
    int snt_fixed;
} p;


void snt_Board_snt_INIT_CELLS(struct snt_Board *b, int n) {
    int i = 0;
    while (i < n) {
        push(&(b->cells), &p);
        ++i;
    }
}

#endif
