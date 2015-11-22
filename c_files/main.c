#include "headers/all_headers.h"

int main() {
    int y = 100;
    int z = 50;
    printf("%d\n", 5);
    Sen_int *x = (Sen_int *) construct_int(&y);
    __auto_type f = CONSTRUCT_INT(z);
    printf("%d\n", 5);
    __auto_type val = *x;
    __auto_type valp = &val;
    free(x);
    x = (typeof(x)) ADD(valp, f);
    printf("\n");
    PRINT_OBJECT(x);
    printf("\n");
    printf("%d\n", 5);
    free(x);
    free(f);
    return 0;
}
