#include "headers/all_headers.h"

/*
Sen_array *construct_array(Sen_object *val[], int len) {
    Sen_array *ret = malloc (sizeof(Sen_array));
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

Sen_array *construct_array(int len) {
    Sen_array *ret = malloc(sizeof(Sen_array));
    ret->len = len;
    ret->bound = false;
    ret->arr = malloc(sizeof(Sen_array *) * len);
    ret->print_sep=' ';
    return ret;
}

void destruct_array(Sen_array *self) {
    for (int i=0; i<self->len; i++) {
        free(self->arr[i]);
    }
    free(self);
}

Sen_array *copy_array(Sen_array *other) {
    Sen_array *ret = malloc(sizeof(Sen_array));
    ret->len = other->len;
    ret->arr = malloc(sizeof (Sen_array *) * ret->len);
    for (int i=0; i<ret->len; i++) {
        ret->arr[i] = COPY(other->arr[i]);
    }
    return ret;
}

Sen_object *access_array(Sen_array *self, Sen_int *index) {
    return self->arr[index->val];
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
    destruct_array,
    copy_array,
    access_array,
    add_array
};

Sen_array_class Sen_array_class_ = {
    &Sen_object_class_,
    &Sen_array_vtable_,
};



