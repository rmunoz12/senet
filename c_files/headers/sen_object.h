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
    void (*print) (Sen_object *);
    Sen_object *(*construct) (void *);
    void (*destruct) (Sen_object *);
    Sen_object *(*copy) (Sen_object *);
};

//static Sen_object_vtable _Sen_object_vtable;

struct Sen_object_class {
    Sen_object_vtable *tablep;
};

//static Sen_object_class _Sen_object_class;

struct Sen_object {
    bool bound;
    Sen_object_class *classp;
};

extern Sen_object_class Sen_object_class_;
extern Sen_object_vtable Sen_object_vtable_;

void print_object (Sen_object *);
Sen_object * construct_object (void *);
void destruct_object (Sen_object *);
Sen_object * copy_object (Sen_object *);

#define PRINT(self) {                                               \
        typeof(self) __temp__ = self;                               \
        __temp__-> classp-> tablep-> print(((Sen_object *)__temp__));  \
}

#define DESTRUCT(self) ({                                   \
            __auto_type __temp__ = self;                    \
            __temp__->classp->tablep->destruct(__temp__);   \
        })

#define COPY(self) ({                                                   \
            typeof(self) __temp__ = self;                               \
            (typeof (__temp__)) __temp__->classp->tablep->copy(__temp__); \
        })

#endif
