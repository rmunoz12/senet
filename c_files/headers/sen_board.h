#include "sen_object.h"
#include "sen_basic_type.h"
#include "sen_int.h"
#include "sen_array.h"

#ifndef SEN_BOARD_H
#define SEN_BOARD_H

struct Sen_board_vtable;
typedef struct Sen_board_vtable Sen_board_vtable;

struct Sen_board_class;
typedef struct Sen_board_class Sen_board_class;

struct Sen_board;
typedef struct Sen_board Sen_board;

struct Sen_board_vtable {
    void (*print) (Sen_object *);
    //Sen_board *(*construct) (Sen_object **, int);
    Sen_board *(*construct) (int);
    void (*destruct) (Sen_board *);
    Sen_board *(*copy) (Sen_board *);
    int *(*index) (Sen_array *);
};

struct Sen_board_class {
    Sen_object_class *superp;
    Sen_board_vtable *tablep;
};

struct Sen_board {
    bool bound;
    Sen_board_class *classp;
    Sen_object *superp;
    Sen_array *data;
    int len;
    char print_sep;
};

extern Sen_board_class Sen_board_class_;
extern Sen_board_vtable Sen_board_vtable_;

//Sen_board *construct_board (Sen_object **, int);
Sen_board *construct_board (int);
int board_index (Sen_array *);

#define CONSTRUCT_BOARD(array) ({                                       \
            __auto_type input_arr = array;                              \
            __auto_type __temp_board__ = construct_board(input_arr->len); \
            __temp_board__->len=input_arr->len;                         \
            printf("OKAY\n");                                           \
            __temp_board__->data=COPY(input_arr);                       \
            if (!input_arr->bound) {                                    \
                DESTRUCT(input_arr);                                    \
            }                                                           \
            __temp_board__;                                             \
        })
#endif

