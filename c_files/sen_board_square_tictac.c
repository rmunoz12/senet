#include "headers/all_headers.h"

/*
Sen_board_square *construct_board_square(Sen_object *val[], int len) {
    Sen_board_square *ret = malloc (sizeof(Sen_board_square));
    ret->len = len
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

Sen_board_square *construct_board_square(int len) {
    Sen_board_square *ret = malloc(sizeof(Sen_board_square));
    ret->classp = &Sen_board_square_class_;
    ret->len   = len;
    ret->bound = false;
    ret->data  = construct_array(len);
    ret->print_sep = ' ';
    printf("hi\n");
    return ret;
}

void destruct_board_square(Sen_board_square *self) {
    DESTRUCT(self->data);
    free(self);
}


Sen_board_square *copy_board_square(Sen_board_square *other) {
    Sen_board_square *ret = malloc(sizeof(Sen_board_square));
    ret->len = other->len;
    ret->data = COPY(other->data);
    return ret;
}

int index_board_square(Sen_array *coord, int len) {
    int x = ((Sen_int *)coord->arr[0])->val;
    int y = ((Sen_int *)coord->arr[1])->val;
    return x + y % len;
}

Sen_board_square_vtable Sen_board_square_vtable_ = {
    print_object,
    construct_board_square,
    destruct_board_square,
    copy_board_square,
    index_board_square
};

Sen_board_square_class Sen_board_square_class_ = {
    &Sen_object_class_,
    &Sen_board_square_vtable_,
};



