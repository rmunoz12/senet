#include "headers/all_headers.h"

Sen_object *access_array(Sen_array *self, Sen_int *index) {
    return self->arr[index->val];
}

Sen_array *construct_array(Sen_object **val) {
    Sen_array *ret = malloc (sizeof(Sen_array *));
    ret->len = sizeof(val);
    ret->arr = malloc(ret->len);
    for (int i=0; i<ret->len/sizeof(Sen_object *); i++) {
        if (!val[i]->bound) {
            ret->arr[i] = val[i];
            val[i]->bound=true;
        } else {
            ret->arr[i]=COPY(val[i]);
        }
    }
    ret->bound=false;
    return ret;
}

Sen_array *add_array(Sen_array *x, Sen_array *y) {
    // Need to check that types are the same
    Sen_array *ret = malloc(sizeof(ret));
    *ret=*x;
    return ret;
}

Sen_array_vtable Sen_array_vtable_ = {
    print_object,
    construct_array,
    access_array,
    add_array
};

Sen_array_class Sen_array_class_ = {
    &Sen_object_class_,
    &Sen_array_vtable_,
};


