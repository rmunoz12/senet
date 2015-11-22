//#include "stdio.h"
//#include "stdlib.h"

#ifndef SEN_OBJECT_H
#define SEN_OBJECT_H

struct Sen_object_vtable;
typedef struct Sen_object_vtable Sen_object_vtable;

struct Sen_object_class;
typedef struct Sen_object_class Sen_object_class;

struct Sen_object;
typedef struct Sen_object Sen_object;

struct Sen_object_vtable {
    void (*print) (Sen_object *self);
};

//static Sen_object_vtable _Sen_object_vtable;

struct Sen_object_class {
    Sen_object_vtable *table;
};

//static Sen_object_class _Sen_object_class;

struct Sen_object {
    Sen_object_class *class;
};

#endif
