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
    char *snt_s;
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

    struct snt_Piece *dummy_piece = malloc(sizeof(struct snt_Piece));
    dummy_piece->snt_owner = -1;
    dummy_piece->snt_fixed = 0;
    dummy_piece->snt_s = " ";

    new_Sen_list(&(b->cells), sizeof(struct snt_Piece));
    new_Sen_list(&(b->occupied), sizeof(bool));
    while (i < n) {
        push(&(b->cells), dummy_piece);
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
        ++i;
    }
    return true;
}

bool snt_Board_snt_remove(struct snt_Board *b, int x) {
    bool *elem = (bool *) list_elem(&(b->occupied), x);
    if (*elem) {
        *elem = false;
        return true;
    } else {
        return false;
    }
}

bool snt_Board_snt_place(struct snt_Board *b, struct snt_Piece *p, int x) {
    bool *occ_elem = (bool *) list_elem(&(b->occupied), x);
    if (*occ_elem) {
        return false;
    }
    *occ_elem = true;
    replace(&(b->cells), p, x);
    return true;
}

int snt_Board_snt_toi(struct snt_Board *b, Sen_list *list) {
    return 0; // must be overwritten in child classes
}

int snt_Rect_snt_toi(struct snt_Rect *b, Sen_list *list) {
    // printf("DEBUG: snt_Rect_snt_toi\n");
    // printf("DEBUG: converting: "); printList(list, printInt); printf("\n");

    int x = *((int *) list_elem(list, 0));
    int y = *((int *) list_elem(list, 1));

    // printf("DEBUG: x: %d\n", x);
    // printf("DEBUG: y: %d\n", y);
    // printf("DEBUG: b->y: %d\n", b->y);
    // printf("DEBUG: output: %d\n", (b->y) * y + x);

    return (b->y) * y + x;
}

int snt_Line_snt_toi(struct snt_Rect *b, Sen_list *list) {
    return *((int *) list_elem(list, 0));
}

int snt_Loop_snt_toi(struct snt_Rect *b, Sen_list *list) {
    int x = *((int *) list_elem(list, 0));
    while (x < 0) {
        x += (b->x);
    }
    return *((int *) list_elem(list, 0)) % (b->x);
}

Sen_list snt_Board_snt_tol(struct snt_Board *b, int i) {
    Sen_list list;
    new_Sen_list(&list, sizeof(int));
    return list; // must be overwritten in child classes
}

Sen_list snt_Rect_snt_tol(struct snt_Rect *b, int i) {
    int max_x = b->x;
    Sen_list list;
    new_Sen_list(&list, sizeof(int));

    int y = i / 3;
    int x = i - (max_x * y);

    push(&list, &x);
    push(&list, &y);

    return list;
}

Sen_list snt_Line_snt_tol(struct snt_Rect *b, int i) {
    Sen_list list;
    new_Sen_list(&list, sizeof(int));
    int x = i;
    push(&list, &x);
    return list;
}

Sen_list snt_Loop_snt_tol(struct snt_Rect *b, int i) {
    Sen_list list;
    new_Sen_list(&list, sizeof(int));
    int x = i;
    push(&list, &x);
    return list;
}

#endif
