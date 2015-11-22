#include "headers/all_headers.h"

int main() {
    printf("%d\n", 5);
    Sen_int x;
    x.class = &Sen_int_class_;
    printf("%d\n", 5);
    int y = 6;
    x.class->table->set_val(&x, y);
    printf("%d\n", 5);
    x.class->table->print((Sen_basic_type *)&x);
    printf("%d\n", 5);
    return 0;
}
