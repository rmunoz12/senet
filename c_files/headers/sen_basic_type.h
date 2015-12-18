#include "sen_object.h"

#ifndef SEN_BASIC_TYPE_H
#define SEN_BASIC_TYPE_H

struct Sen_basic_type_vtable;
typedef struct Sen_basic_type_vtable Sen_basic_type_vtable;

struct Sen_basic_type_class;
typedef struct Sen_basic_type_class Sen_basic_type_class;

struct Sen_basic_type;
typedef struct Sen_basic_type Sen_basic_type;

typedef enum {BOOL, INT, STR, UNK} Type;

struct Sen_basic_type_vtable {
    void (*print) (Sen_object *);
    Sen_basic_type *(*construct) (void *);
    void *(*get_val) (Sen_basic_type *);
    void *(*set_val) (Sen_basic_type *, void *);
    Sen_basic_type *(*add) (Sen_basic_type *, Sen_basic_type *);
};

struct Sen_basic_type_class {
    Sen_object_class *superp;
    Sen_basic_type_vtable *tablep;
    Type type;
};

struct Sen_basic_type {
    bool bound;
    Sen_basic_type_class *classp;
    Sen_object *superp;
};

extern Sen_basic_type_class Sen_basic_type_class_;
extern Sen_basic_type_vtable Sen_basic_type_vtable_;

void * get_val_basic_type (Sen_basic_type *);
void * set_val_basic_type (Sen_basic_type *, void *);
Sen_basic_type * add_basic_type (Sen_basic_type *, Sen_basic_type *);

//#define ADD_BASIC_TYPE(x,y, target) x->classp->tablep->add((Sen_basic_type *)x, (Sen_basic_type *)y)
#define ADD_BASIC_TYPE(x,y) ({                  \
    __auto_type  __temp__ = x;\
    __temp__->classp->tablep->add((Sen_basic_type *)__temp__, (Sen_basic_type *)y);\
    })

#endif
