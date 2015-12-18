#include "sen_object.h"
#include "sen_basic_type.h"
#include "sen_int.h"
#include "sen_array.h"

#ifndef SEN_BOARD_SQUARE_H
#define SEN_BOARD_SQUARE_H

struct Sen_board_square_vtable;
typedef struct Sen_board_square_vtable Sen_board_square_vtable;

struct Sen_board_square_class;
typedef struct Sen_board_square_class Sen_board_square_class;

struct Sen_board_square;
typedef struct Sen_board_square Sen_board_square;

struct Sen_board_square_vtable {
    void (*print) (Sen_object *);
    //Sen_board_square *(*construct) (Sen_object **, int);
    Sen_board_square *(*construct) (int);
    void (*destruct) (Sen_board_square *);
    Sen_board_square *(*copy) (Sen_board_square *);
    int (*index) (Sen_array *, int);
};

struct Sen_board_square_class {
    Sen_object_class *superp;
    Sen_board_square_vtable *tablep;
};

struct Sen_board_square {
    bool bound;
    Sen_board_square_class *classp;
    Sen_object *superp;
    Sen_array *data;
    int len;
    char print_sep;
};

extern Sen_board_square_class Sen_board_square_class_;
extern Sen_board_square_vtable Sen_board_square_vtable_;

//Sen_board_square *construct_board_square (Sen_object **, int);
Sen_board_square *construct_board_square (int);
int board_square_index (Sen_array *);

#define CONSTRUCT_BOARD_SQUARE(array) ({                                       \
            __auto_type input_arr = array;                              \
            __auto_type __temp_board_square__ = construct_board_square(input_arr->len); \
            __temp_board_square__->len=input_arr->len;                         \
            printf("OKAY\n");\
            __temp_board_square__->data=COPY(input_arr);                       \
            if (!input_arr->bound) {                                    \
                DESTRUCT(input_arr);                                    \
            }                                                           \
            __temp_board_square__;                                             \
        })
#endif

