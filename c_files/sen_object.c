#include "headers/all_headers.h"

void print_object(Sen_object *self) {

}

Sen_object_vtable Sen_object_vtable_ = {
    print_object,
};

Sen_object_class Sen_object_class_ = {
    &Sen_object_vtable_
};


