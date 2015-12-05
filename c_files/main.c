#include "headers/all_headers.h"

int main() {
    __auto_type x = CONSTRUCT_INT(100);
    x->bound = true;
    __auto_type f = CONSTRUCT_INT(50);
    f->bound=true;
    printf("%d\n", x->val);
    PRINT(x);
    printf("\n");
    PRINT(f);
    printf("\n");
    printf("%d %d\n", x->bound, f->bound);
    x = (typeof(x)) ADD_BASIC_TYPE(x, ((Sen_int *) ADD_BASIC_TYPE(x, f)));
    printf("%d %d\n", x->bound, f->bound);
    x->bound=true;
    PRINT(x);
    printf("\n");
    printf("%d %d\n", x->bound, f->bound);
    free(x);
    free(f);

    __auto_type s = CONSTRUCT_STRING("testing ");
    s->bound=true;
    __auto_type ss = CONSTRUCT_STRING("hooray!!\n");
    ss->bound=true;
    PRINT(((Sen_string *)ADD_BASIC_TYPE(s, ss)));
    PRINT(((Sen_string *)ADD_BASIC_TYPE(s, ((Sen_string *)ADD_BASIC_TYPE(s, ss)))));
    DESTRUCT(((Sen_string *) s));
    DESTRUCT(((Sen_string *) ss));
    return 0;
}
