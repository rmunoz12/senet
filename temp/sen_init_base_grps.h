#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include "sen_linked_list.h"

#ifndef SEN_INIT_BASE_GRPS
#define SEN_INIT_BASE_GRPS

bool OCCUPIED = true;
bool NOT_OCCUPIED = false;

struct snt_Board {
    Sen_list cells;
    Sen_list occupied;
};

struct snt_Piece {
    int snt_owner;
    int snt_fixed;
} p;

struct snt_Rect{
    Sen_list cells;
    Sen_list occupied;
    int x;
    int y;
};
struct snt_Line{
    Sen_list cells;
    Sen_list occupied;
    int x;
};
struct snt_Loop{
    Sen_list cells;
    Sen_list occupied;
    int x;
};
struct snt_Hex{
    Sen_list cells;
    Sen_list occupied;
    int x;
};


void snt_Board_snt_INIT_CELLS(struct snt_Board *b, int n) {
    int i = 0;
    while (i < n) {
        push(&(b->cells), &p);
        push(&(b->occupied), &NOT_OCCUPIED);
        ++i;
    }
}

int snt_Board_snt_owns(struct snt_Board *b, int i) {
    struct snt_Piece *p = (struct snt_Piece *) list_elem(&(b->cells), i);
    return p->snt_owner;
}

bool snt_Board_snt_full(struct snt_Board *b) {
    int i = 0;
    while (i < b->occupied.len) {
        bool elem = *((bool *) list_elem(&(b->occupied), i));
        if (elem == false) {
            return false;
        }
    }
    return true;
}

void snt_Board_snt_remove(struct snt_Board *b, int x) {
    bool *elem = (bool *) list_elem(&(b->occupied), x);
    *elem = false;
}

int snt_Board_snt_toi(struct snt_Board *b, Sen_list *list) {
    return 0; // must be overwritten in child classes
}

int snt_Rect_snt_toi(struct snt_Rect *b, Sen_list *list) {
    int x = *((int *) list_elem(list, 0));
    int y = *((int *) list_elem(list, 1));
    return (b->y) * y + x;
}


#endif
