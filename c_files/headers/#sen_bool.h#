#include "sen_basic_type.h"

#ifndef SEN_BOOL_H
#define SEN_BOOL_H

struct Sen_bool_vtable;
typedef struct Sen_bool_vtable Sen_bool_vtable;

struct Sen_bool_class;
typedef struct Sen_bool_class Sen_bool_class;

struct Sen_bool;
typedef struct Sen_bool Sen_bool;

struct Sen_bool_vtable {
    void (*print) (Sen_object *);
    void *(*get_val) (Sen_basic_type *);
    void *(*set_val) (Sen_basic_type *, void *);
    Sen_bool *(*construct) (bool);
    void (*destruct) (Sen_bool *);
    Sen_basic_type *(*add) (Sen_basic_type *, Sen_basic_type *);
};

struct Sen_bool_class {
    Sen_basic_type_class *superp;
    Sen_bool_vtable *tablep;
    Type type;
};

struct Sen_bool {
    bool bound;
    Sen_bool_class *classp;
    Sen_basic_type *superp;
    bool val;
};

extern Sen_bool_class Sen_bool_class_;
extern Sen_bool_vtable Sen_bool_vtable_;

void print_bool (Sen_object *);
Sen_bool * construct_bool (bool);
void *get_val_bool (Sen_basic_type *);
void *set_val_bool (Sen_basic_type *, void *);

#define CONSTRUCT_BOOL(val) (Sen_bool*) construct_bool(val)

#endif
