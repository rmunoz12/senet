#include "headers/all_headers.h"

/*
Sen_board *construct_board(Sen_object *val[], int len) {
    Sen_board *ret = malloc (sizeof(Sen_board));
    ret->len = len;
    printf("%d %d\n", (int)ret->len, (int)sizeof(Sen_object *));
    ret->arr = malloc(sizeof(typeof (val[0])) * (len));
    for (int i=0; i<len; i++) {
        (ret->arr)[i] = COPY(((typeof (val[i])) val[i]));
        (ret->arr)[i]->bound=true;
        if (!val[i]->bound) {
            DESTRUCT(val[i]);
        }
    }
    ret->bound=false;
    return ret;
}
*/

Sen_board *construct_board(int len) {
    Sen_board *ret = malloc(sizeof(Sen_board));
    ret->len = len;
    ret->bound = false;
    ret->data = construct_array(len);
    ret->print_sep=' ';
    return ret;
}

void destruct_board(Sen_board *self) {
    DESTRUCT(self->data);
    free(self);
}

Sen_board *copy_board(Sen_board *other) {
    Sen_board *ret = malloc(sizeof(Sen_board));
    ret->len = other->len;
    ret->data = COPY(other->data);
    return ret;
}

Sen_board_vtable Sen_board_vtable_ = {
    print_object,
    construct_board,
    destruct_board,
};

Sen_board_class Sen_board_class_ = {
    &Sen_object_class_,
    &Sen_board_vtable_,
};



