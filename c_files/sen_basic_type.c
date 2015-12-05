#include "headers/all_headers.h"

void *get_val_basic_type(Sen_basic_type *self) {
    int *ret = malloc(sizeof *ret);
    *ret=((Sen_int *) self)->val;
    return ret;
}

void *set_val_basic_type(Sen_basic_type *self, void *val) {

    //self->val=val;
    return val;
}

Sen_basic_type *construct_basic_type(void *val) {
    Sen_basic_type *ret = malloc(sizeof(ret));
    return ret;
}

Sen_basic_type *add_basic_type(Sen_basic_type *x, Sen_basic_type *y) {
    // Need to check that types are the same
    Sen_basic_type *ret = malloc(sizeof(ret));
    *ret=*x;
    return ret;
}

Sen_basic_type_vtable Sen_basic_type_vtable_ = {
    print_object,
    construct_basic_type,
    get_val_basic_type,
    set_val_basic_type,
    add_basic_type
};

Sen_basic_type_class Sen_basic_type_class_ = {
    &Sen_object_class_,
    &Sen_basic_type_vtable_,
    UNK
};


