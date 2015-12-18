#include "headers/all_headers.h"

int main() {
    /*
    __auto_type x = CONSTRUCT_INT(100);
    x->bound = true;
    __auto_type f = CONSTRUCT_INT(50);
    f->bound=true;
    printf("%d\n", x->val);
    PRINT(x);
    printf("\n");
    PRINT(f);
    printf("%d %d\n", x->bound, f->bound);
    {
        typeof(x) __temp__ = (typeof(x)) ADD_BASIC_TYPE(x, ((Sen_int *) ADD_BASIC_TYPE(x, f)));
        free(x);
        x = __temp__;
        x->bound=true;
    }
    printf("%d %d\n", x->bound, f->bound);
    x->bound=true;
    PRINT(x);
    printf("\n");
    printf("%d %d\n", x->bound, f->bound);
    DESTRUCT(x);
    DESTRUCT(f);
    */
    __auto_type s = CONSTRUCT_STRING("tttesting ");
    s->bound=true;
    __auto_type ss = CONSTRUCT_STRING("hooray!!\n");
    ss->bound=true;
    //PRINT(arr_[0]);

    PRINT(((Sen_string *) ss));
    PRINT(((Sen_string *)ADD_BASIC_TYPE(s, ss)));
    PRINT(((Sen_string *)ADD_BASIC_TYPE(((Sen_string *)ADD_BASIC_TYPE(s, ss)), ((Sen_string *)ADD_BASIC_TYPE(s, ss)))));
    DESTRUCT(((Sen_string *) s));
    DESTRUCT(((Sen_string *) ss));
    //Sen_array *arr = (Sen_array_vtable_.construct(arr_, ARRAY_SIZE(arr_)));
    //Sen_array *arr = (Sen_array_vtable_.construct((Sen_int*[]){CONSTRUCT_INT(100), CONSTRUCT_INT(50)}, 2));
    Sen_array *arr = CONSTRUCT_ARRAY(((Sen_int*[]){CONSTRUCT_INT(100), CONSTRUCT_INT(50)}), 2);
    arr->bound = true;
    //Sen_array *arr = CONSTRUCT_ARRAY(arr_, 2);
    printf("%d %d\n", ((Sen_int *)((arr->arr)[0]))->val, arr->len);
    Sen_board *board = CONSTRUCT_BOARD(arr);
    printf("%d %d\n", ((Sen_int *)((arr->arr)[0]))->val, arr->len);
    DESTRUCT(arr);
    printf("OKAY\n");
    DESTRUCT(board);
    printf("OKAY\n");
    //DESTRUCT(((Sen_array *) arr));
    return 0;
}
