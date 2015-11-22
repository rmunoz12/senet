//#include "stdio.h"
//#include "stdlib.h"
#include "sen_object.h"

#ifndef SEN_BASIC_TYPE_H
#define SEN_BASIC_TYPE_H

struct Sen_basic_type_vtable;
typedef struct Sen_basic_type_vtable Sen_basic_type_vtable;

struct Sen_basic_type_class;
typedef struct Sen_basic_type_class Sen_basic_type_class;

struct Sen_basic_type;
typedef struct Sen_basic_type Sen_basic_type;

typedef enum {BOOL, INT, STR} Type;

struct Sen_basic_type_vtable {
    void (*print) (Sen_basic_type *self);
    void *(*get_val) (Sen_basic_type *self);
    void *(*set_val) (Sen_basic_type *self, void *val);
    Sen_basic_type *(*add) (Sen_basic_type *x, Sen_basic_type *y);
};

struct Sen_basic_type_class {
    Sen_object_class *super;
    Sen_basic_type_vtable *table;
    Type type;
};

struct Sen_basic_type {
    Sen_basic_type_class *class;
};

#endif
