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
    void (*print) (Sen_object *);
    Sen_basic_type *(*construct) (void *);
    void *(*get_val) (Sen_basic_type *);
    void *(*set_val) (Sen_basic_type *, void *);
    Sen_basic_type *(*add) (Sen_basic_type *, Sen_basic_type *);
};

struct Sen_int_class {
    Sen_basic_type_class *superp;
    Sen_int_vtable *tablep;
    Type type;
};

struct Sen_int {
    Sen_int_class *classp;
    int val;
};

extern Sen_int_class Sen_int_class_;
extern Sen_int_vtable Sen_int_vtable_;

void print_int (Sen_object *);
Sen_basic_type * construct_int (void *);
void *get_val_int (Sen_basic_type *);
void *set_val_int (Sen_basic_type *, void *);

#define CONSTRUCT_INT(val) (Sen_int*) construct_int(& val)

#endif
