#include "headers/all_headers.h"

Sen_string *construct_string(char *val) {
    int l = strlen(val);
    Sen_string *ret = malloc(sizeof(Sen_string));
    ret->classp = &Sen_string_class_;
    ret->val = (char *)malloc(l+1);
    strncpy(ret->val, val, l+1);
    ret->bound = false;
    return ret;
}

void destruct_string(Sen_string *self) {
    free(self->val);
    self->val = NULL;
    free(self);
    self=NULL;
}


Sen_string *copy_string(Sen_string *other) {
    return construct_string(other->val);
}

void print_string(Sen_object *self) {
    printf("%s", ((Sen_string *) self)->val);
    if (!self->bound) {
        destruct_string((Sen_string *) self);
    }
}

void *get_val_string(Sen_basic_type *self) {
    char **ret = malloc(sizeof *ret);
    *ret=((Sen_string *) self)->val;
    if (!self->bound) {
        destruct_string((Sen_string *) self);
    }
    return ret;
}

void *set_val_string(Sen_basic_type *self, void *val) {
    ((Sen_string *)self)->val=*(char **)val;
    if (!self->bound) {
        destruct_string((Sen_string *) self);
    }
    return val;
}

Sen_basic_type *add_string(Sen_basic_type *x, Sen_basic_type *y) {
    // Need to check types for safety
    char *new_string = malloc(strlen(((Sen_string*)x)->val) + strlen(((Sen_string*)y)->val) + 1);
    strcpy(new_string, ((Sen_string*) x)->val);
    strcat(new_string, ((Sen_string*) y)->val);
    Sen_string *ret = construct_string(new_string);
    free(new_string);
    if (!x->bound) {
        destruct_string((Sen_string*) x);
    }
    if (!y->bound) {
        destruct_string((Sen_string*) y);
    }
    return (Sen_basic_type *) ret;
}

Sen_string_vtable Sen_string_vtable_ = {
    print_string,
    construct_string,
    destruct_string,
    copy_string,
    get_val_string,
    set_val_string,
    add_string
};

//Sen_basic_type_class temp; /* NEED TO FIX */
Sen_string_class Sen_string_class_ = {
    &Sen_basic_type_class_,
    &Sen_string_vtable_,
    STR
};