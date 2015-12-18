#include "sen_object.h"
#include "sen_basic_type.h"
#include "sen_int.h"
#include "sen_array.h"

#ifndef SEN_BOARD_SQUARE_TICTAC_H
#define SEN_BOARD_SQUARE_TICTAC_H

struct Sen_board_square_tictac_vtable;
typedef struct Sen_board_square_tictac_vtable Sen_board_square_tictac_vtable;

struct Sen_board_square_tictac_class;
typedef struct Sen_board_square_tictac_class Sen_board_square_tictac_class;

struct Sen_board_square_tictac;
typedef struct Sen_board_square_tictac Sen_board_square_tictac;

struct Sen_board_square_tictac_vtable {
    void (*print) (Sen_object *);
    //Sen_board_square_tictac *(*construct) (Sen_object **, int);
    Sen_board_square_tictac *(*construct) (int);
    void (*destruct) (Sen_board_square_tictac *);
    Sen_board_square_tictac *(*copy) (Sen_board_square_tictac *);
    int (*index) (Sen_array *, int);
};

struct Sen_board_square_tictac_class {
    Sen_object_class *superp;
    Sen_board_square_tictac_vtable *tablep;
};

struct Sen_board_square_tictac {
    bool bound;
    Sen_board_square_tictac_class *classp;
    Sen_object *superp;
    Sen_array *data;
    int len;
    char print_sep;
};

extern Sen_board_square_tictac_class Sen_board_square_tictac_class_;
extern Sen_board_square_tictac_vtable Sen_board_square_tictac_vtable_;

//Sen_board_square_tictac *construct_board_square_tictac (Sen_object **, int);
Sen_board_square_tictac *construct_board_square_tictac (int);
int board_square_tictac_index (Sen_array *);

#define CONSTRUCT_BOARD_SQUARE_TICTAC(array) ({                         \
            __auto_type input_arr = array;                              \
            __auto_type __temp_board_square_tictac__ = construct_board_square_tictac(input_arr->len); \
            __temp_board_square_tictac__->len=input_arr->len;           \
            printf("OKAY\n");                                           \
            __temp_board_square_tictac__->data=COPY(input_arr);         \
            if (!input_arr->bound) {                                    \
                DESTRUCT(input_arr);                                    \
            }                                                           \
            __temp_board_square_tictac__;                               \
        })
#endif

