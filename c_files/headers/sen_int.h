#include "sen_basic_type.h"

#ifndef SEN_INT_H
#define SEN_INT_H

struct Sen_int_vtable;
typedef struct Sen_int_vtable Sen_int_vtable;

struct Sen_int_class;
typedef struct Sen_int_class Sen_int_class;

struct Sen_int;
typedef struct Sen_int Sen_int;

struct Sen_int_vtable {
    void (*print) (Sen_basic_type *self);
    void *(*get_val) (Sen_basic_type *self);
    int (*set_val) (Sen_int *self, int val);
    Sen_basic_type *(*add) (Sen_basic_type *x, Sen_basic_type *y);
};

struct Sen_int_class {
    Sen_basic_type_class *super;
    Sen_int_vtable *table;
    Type type;
};

struct Sen_int {
    Sen_int_class *class;
    int val;
};

extern Sen_int_class Sen_int_class_;
extern Sen_int_vtable Sen_int_vtable_;

#endif
