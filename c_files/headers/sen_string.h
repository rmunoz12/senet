#include "sen_basic_type.h"

#ifndef SEN_STRING_H
#define SEN_STRING_H

struct Sen_string_vtable;
typedef struct Sen_string_vtable Sen_string_vtable;

struct Sen_string_class;
typedef struct Sen_string_class Sen_string_class;

struct Sen_string;
typedef struct Sen_string Sen_string;

struct Sen_string_vtable {
    void (*print) (Sen_object *);
    Sen_string *(*construct) (char *);
    void (*destruct) (Sen_string *);
    Sen_string *(*copy) (Sen_string *);
    void *(*get_val) (Sen_basic_type *);
    void *(*set_val) (Sen_basic_type *, void *);
    Sen_basic_type *(*add) (Sen_basic_type *, Sen_basic_type *);
};

struct Sen_string_class {
    Sen_basic_type_class *superp;
    Sen_string_vtable *tablep;
    Type type;
};

struct Sen_string {
    bool bound;
    Sen_string_class *classp;
    Sen_basic_type *superp;
    char *val;
};

extern Sen_string_class Sen_string_class_;
extern Sen_string_vtable Sen_string_vtable_;

void print_string (Sen_object *);
Sen_string * construct_string (char *);
void destruct (Sen_string *);
Sen_string * copy_string  (Sen_string *);
void *get_val_string (Sen_basic_type *);
void *set_val_string (Sen_basic_type *, void *);


#define CONSTRUCT_STRING(val) ((Sen_string*) construct_string(val))

#endif
