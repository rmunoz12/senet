#include "sen_object.h"
#include "sen_basic_type.h"
#include "sen_int.h"

#ifndef SEN_ARRAY_H
#define SEN_ARRAY_H

#if !defined(ARRAY_SIZE)
    #define ARRAY_SIZE(x) (sizeof((x)) / sizeof((x)[0]))
#endif

struct Sen_array_vtable;
typedef struct Sen_array_vtable Sen_array_vtable;

struct Sen_array_class;
typedef struct Sen_array_class Sen_array_class;

struct Sen_array;
typedef struct Sen_array Sen_array;

struct Sen_array_vtable {
    void (*print) (Sen_object *);
    //Sen_array *(*construct) (Sen_object **, int);
    Sen_array *(*construct) (int);
    void (*destruct) (Sen_array *);
    Sen_array *(*copy) (Sen_array *);
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
    Sen_object **arr;
    int len;
    char print_sep;
};

extern Sen_array_class Sen_array_class_;
extern Sen_array_vtable Sen_array_vtable_;

//Sen_array *construct_array (Sen_object **, int);
Sen_array *construct_array (int);
void destruct_array (Sen_array *);
Sen_array *copy_array (Sen_array *);
Sen_object *access_array (Sen_array *, Sen_int *);
Sen_array *concat_array (Sen_array *, Sen_array *);

#define CONCAT_ARRAY(x,y) ({\
    __auto_type __temp__ = x;\
    __temp__->classp->tablep->concat((Sen_array *)temp, (Sen_array *)y);\
        })

//#define CONSTRUCT_ARRAY(x) ((Sen_array *) construct_array(x))
#define CONSTRUCT_ARRAY(array, length) ({                               \
            __auto_type input_arr = array;                              \
            __auto_type __temp_arr__ = construct_array(length);         \
            __temp_arr__->len=length;                                   \
            for (int i=0; i<length; i++) {                              \
                __auto_type __temp_elem__ = input_arr[i];               \
                __temp_arr__->arr[i] = (Sen_object *)COPY(__temp_elem__);\
                if (!__temp_elem__->bound) {                            \
                    DESTRUCT(__temp_elem__);                            \
                }                                                       \
            }                                                           \
            __temp_arr__;                                               \
        })
#endif