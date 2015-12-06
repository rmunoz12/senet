#include "sen_object.h"
#include "sen_basic_type.h"
#include "sen_int.h"

#ifndef SEN_ARRAY_H
#define SEN_ARRAY_H

struct Sen_array_vtable;
typedef struct Sen_array_vtable Sen_array_vtable;

struct Sen_array_class;
typedef struct Sen_array_class Sen_array_class;

struct Sen_array;
typedef struct Sen_array Sen_array;

struct Sen_array_vtable {
    void (*print) (Sen_object *);
    Sen_array *(*construct) (Sen_object **);
    Sen_object *(*access) (Sen_array *, Sen_int *);
    Sen_array *(*concat) (Sen_array *, Sen_array *);
};

struct Sen_array_class {
    Sen_object_class *superp;
    Sen_array_vtable *tablep;
};

struct Sen_array {
    bool bound;
    Sen_array_class *classp;
    Sen_object *superp;
    void **arr;
    int len;
    char print_sep;
};

extern Sen_array_class Sen_array_class_;
extern Sen_array_vtable Sen_array_vtable_;

Sen_array *construct_array (Sen_object **);
Sen_object *access_array (Sen_array *, Sen_int *);
Sen_array *concat_array (Sen_array *, Sen_array *);

#define CONCAT_ARRAY(x,y) x->classp->tablep->concat((Sen_array *)x, (Sen_array *)y)
#define CONSTRUCT_ARRAY(x) ((Sen_array *) construct_array(x))

#endif
