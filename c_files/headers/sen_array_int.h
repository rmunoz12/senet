#include "sen_object.h"
#include "sen_basic_type.h"
#include "sen_int.h"

#ifndef SEN_ARRAY_INT_H
#define SEN_ARRAY_INT_H

#if !defined(ARRAY_INT_SIZE)
    #define ARRAY_INT_SIZE(x) (sizeof((x)) / sizeof((x)[0]))
#endif

struct Sen_array_int_vtable;
typedef struct Sen_array_int_vtable Sen_array_int_vtable;

struct Sen_array_int_class;
typedef struct Sen_array_int_class Sen_array_int_class;

struct Sen_array_int;
typedef struct Sen_array_int Sen_array_int;

struct Sen_array_int_vtable {
    void (*print) (Sen_object *);
    //Sen_array_int *(*construct) (Sen_object **, int);
    Sen_array_int *(*construct) (int);
    void (*destruct) (Sen_array_int *);
    Sen_array_int *(*copy) (Sen_array_int *);
    Sen_int *(*access) (Sen_array_int *, Sen_int *);
    Sen_array_int *(*concat) (Sen_array_int *, Sen_array_int *);
};

struct Sen_array_int_class {
    Sen_object_class *superp;
    Sen_array_int_vtable *tablep;
};

struct Sen_array_int {
    bool bound;
    Sen_array_int_class *classp;
    Sen_object *superp;
    Sen_int **arr;
    int len;
    char print_sep;
};

extern Sen_array_int_class Sen_array_int_class_;
extern Sen_array_int_vtable Sen_array_int_vtable_;

//Sen_array_int *construct_array_int (Sen_object **, int);
Sen_array_int *construct_array_int (int);
void destruct_array_int (Sen_array_int *);
Sen_array_int *copy_array_int (Sen_array_int *);
Sen_int *access_array_int (Sen_array_int *, Sen_int *);
Sen_array_int *concat_array_int (Sen_array_int *, Sen_array_int *);

#define CONCAT_ARRAY_INT(x,y) ({\
    __auto_type __temp__ = x;\
    __temp__->classp->tablep->concat((Sen_array_int *)temp, (Sen_array_int *)y);\
        })

//#define CONSTRUCT_ARRAY_INT(x) ((Sen_array_int *) construct_array_int(x))
#define CONSTRUCT_ARRAY_INT(array_int, length) ({                               \
            __auto_type input_arr = array_int;                              \
            __auto_type __temp_arr__ = construct_array_int(length);         \
            __temp_arr__->len=length;                                   \
            for (int i=0; i<length; i++) {                              \
                __auto_type __temp_elem__ = input_arr[i];         \
                __auto_type __temp_cpy__ = COPY(input_arr[i]);    \
                __temp_arr__->arr[i] = __temp_cpy__;\
                if (!__temp_elem__->bound) {                            \
                    DESTRUCT(__temp_elem__);                            \
                }                                                       \
            }                                                           \
            __temp_arr__;                                               \
        })
#endif