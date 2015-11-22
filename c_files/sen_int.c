#include "headers/all_headers.h"

void print(Sen_basic_type *self) {
    printf("%d", ((Sen_int *) self)->val);
}

void *get_val(Sen_basic_type *self) {
    int *ret = malloc(sizeof *ret);
    *ret=((Sen_int *) self)->val;
    return ret;
}

int set_val(Sen_int *self, int val) {
    printf("%d\n", 1);
    self->val=val;
    return val;
}

Sen_basic_type *sum(Sen_basic_type *x, Sen_basic_type *y) {
    Sen_basic_type *ret = malloc(sizeof ret);
    return ret;
}

Sen_int_vtable Sen_int_vtable_ = {
    print,
    get_val,
    set_val,
    sum
};

Sen_int_class Sen_int_class_;


