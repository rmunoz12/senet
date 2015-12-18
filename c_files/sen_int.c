#include "headers/all_headers.h"

Sen_int *construct_int(int val) {
    Sen_int *ret = malloc(sizeof(Sen_int));
    ret->classp = &Sen_int_class_;
    ret->val = val;
    ret->bound = false;
    return ret;
}

void destruct_int(Sen_int *self) {
    free(self);
}

Sen_int *copy_int(Sen_int *self) {
    Sen_int *ret = construct_int(self->val);
    return ret;
}

void print_int(Sen_object *self) {
    printf("%d", ((Sen_int *) self)->val);
    if (!((Sen_int *)self)->bound) {
        free(self);
    }
}

void *get_val_int(Sen_basic_type *self) {
    int *ret = malloc(sizeof *ret);
    *ret=((Sen_int *) self)->val;
    if (!self->bound) {
        free(self);
    }
    return ret;
}

void *set_val_int(Sen_basic_type *self, void *val) {
    ((Sen_int *)self)->val=*(int*)val;
    if (!self->bound) {
        free(self);
    }
    return val;
}

Sen_basic_type *add_int(Sen_basic_type *x, Sen_basic_type *y) {
    // Need to check types for safety
    Sen_int *ret = construct_int(((Sen_int *)x)->val);
    ret->val+=((Sen_int *) y)->val;
    if (!x->bound) {
        free(x);
    }
    if (!y->bound) {
        free(y);
    }
    return (Sen_basic_type *) ret;
}

Sen_int_vtable Sen_int_vtable_ = {
    print_int,
    get_val_int,
    set_val_int,
    construct_int,
    destruct_int,
    copy_int,
    add_int
};

//Sen_basic_type_class temp; /* NEED TO FIX */
Sen_int_class Sen_int_class_ = {
    &Sen_basic_type_class_,
    &Sen_int_vtable_,
    INT
};