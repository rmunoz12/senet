#include "headers/all_headers.h"

Sen_bool *construct_bool(bool val) {
    Sen_bool *ret = malloc(sizeof(Sen_bool));
    ret->classp = &Sen_bool_class_;
    ret->val = val;
    ret->bound = false;
    return ret;
}

void destruct_bool(Sen_bool *self) {
    free(self);
}

void print_bool(Sen_object *self) {
    if (((Sen_bool*) self)->val==true) {
        printf("True");
    } else {
        printf("False");
    }
    if (!((Sen_bool *)self)->bound) {
        free(self);
    }
}

void *get_val_bool(Sen_basic_type *self) {
    bool *ret = malloc(sizeof *ret);
    *ret=((Sen_bool *) self)->val;
    if (!((Sen_bool *)self)->bound) {
        free(self);
    }
    return ret;
}

void *set_val_bool(Sen_basic_type *self, void *val) {
    ((Sen_bool *)self)->val=*(bool*)val;
    if (!((Sen_bool *)self)->bound) {
        free(self);
    }
    return val;
}

Sen_basic_type *add_bool(Sen_basic_type *x, Sen_basic_type *y) {
    // Need to check types for safety
    Sen_bool *ret = construct_bool(((Sen_bool *)x)->val);
    ret->val = ret->val != ((Sen_bool *) y)->val;
    if (!x->bound) {
        free(x);
    }
    if (!y->bound) {
        free(y);
    }
    return (Sen_basic_type *) ret;
}

Sen_bool_vtable Sen_bool_vtable_ = {
    print_bool,
    get_val_bool,
    set_val_bool,
    construct_bool,
    destruct_bool,
    add_bool
};

//Sen_basic_type_class temp; /* NEED TO FIX */
Sen_bool_class Sen_bool_class_ = {
    &Sen_basic_type_class_,
    &Sen_bool_vtable_,
    BOOL
};


