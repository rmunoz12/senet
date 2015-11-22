#include "headers/all_headers.h"

Sen_basic_type *construct_int(void *val) {
    Sen_int *ret = malloc(sizeof(Sen_basic_type));
    ret->classp = &Sen_int_class_;
    ret->val = *(int *) val;
    return (Sen_basic_type *) ret;
}

void print_int(Sen_object *self) {
    printf("%d", ((Sen_int *) self)->val);
}

void *get_val_int(Sen_basic_type *self) {
    int *ret = malloc(sizeof *ret);
    *ret=((Sen_int *) self)->val;
    return ret;
}

void *set_val_int(Sen_basic_type *self, void *val) {
    ((Sen_int *)self)->val=*(int*)val;
    return val;
}

Sen_basic_type *add_int(Sen_basic_type *x, Sen_basic_type *y) {
    // Need to check types for safety
    Sen_int *ret = malloc(sizeof ret);
    *ret=*(Sen_int *)x;
    ret->val+=((Sen_int *) y)->val;
    return (Sen_basic_type *) ret;
}

Sen_int_vtable Sen_int_vtable_ = {
    print_int,
    construct_int,
    get_val_int,
    set_val_int,
    add_int
};

Sen_basic_type_class temp; /* NEED TO FIX */
Sen_int_class Sen_int_class_ = {
    &temp,
    &Sen_int_vtable_,
    INT
};


