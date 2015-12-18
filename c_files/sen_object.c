#include "headers/all_headers.h"

void print_object(Sen_object *self) {

}

Sen_object *construct_object(void *val) {
    Sen_object *ret = malloc (sizeof (Sen_object *));
    return ret;
}

void destruct_object(Sen_object *val) {
    free(val);
}

Sen_object *copy_object(Sen_object *self) {
    Sen_object *ret=malloc(sizeof(Sen_object *));
    ret->classp=&Sen_object_class_;
    return ret;
}

Sen_object_vtable Sen_object_vtable_ = {
    print_object,
    construct_object,
    destruct_object,
    copy_object
};

Sen_object_class Sen_object_class_ = {
    NULL,
    &Sen_object_vtable_
};


