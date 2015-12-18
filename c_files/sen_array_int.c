#include "headers/all_headers.h"

/*
Sen_array_int *construct_array_int(Sen_object *val[], int len) {
    Sen_array_int *ret = malloc (sizeof(Sen_array_int));
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

Sen_array_int *construct_array_int(int len) {
    Sen_array_int *ret = malloc(sizeof(Sen_array_int));
    ret->len = len;
    ret->classp = &Sen_array_int_class_;
    ret->bound = false;
    ret->arr = malloc(sizeof(Sen_int *) * len);
    ret->print_sep=' ';
    return ret;
}

void destruct_array_int(Sen_array_int *self) {
    for (int i=0; i<self->len; i++) {
        free(self->arr[i]);
    }
    free(self);
}

Sen_array_int *copy_array_int(Sen_array_int *other) {
    Sen_array_int *ret = construct_array_int(other->len);
    for (int i=0; i<ret->len; i++) {
        printf("%d %d\n", other->len, ((Sen_int *)(other->arr[i]))->val);
        printf("OKAY1\n");
        ret->arr[i] = COPY(((other->arr)[i]));
        printf("OKAY2\n");
    }
    printf("OKAY1\n");
    return ret;
}

Sen_int *access_array_int(Sen_array_int *self, Sen_int *index) {
    return self->arr[index->val];
}

Sen_array_int *add_array_int(Sen_array_int *x, Sen_array_int *y) {
    // Need to check that types are the same
    Sen_array_int *ret = malloc(sizeof(ret));
    *ret=*x;
    return ret;
}

Sen_array_int_vtable Sen_array_int_vtable_ = {
    print_object,
    construct_array_int,
    destruct_array_int,
    copy_array_int,
    access_array_int,
    add_array_int
};

Sen_array_int_class Sen_array_int_class_ = {
    &Sen_object_class_,
    &Sen_array_int_vtable_,
};



