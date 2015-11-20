#include "stdio.h"
#include "stdlib.h"

struct Sen_node {
    void *data;
    struct Sen_node *next;
};
typedef Sen_node Sen_node;

/* Function to add a Sen_node at the beginning of Linked List.
   This function expects a pointer to the data to be added
   and size of the data type */
void push(struct Sen_node** head_ref, void *new_data, size_t data_size) {
    // Allocate memory for Sen_node
    struct Sen_node* new_Sen_node = (struct Sen_node*)malloc(sizeof(struct Sen_node));

    new_Sen_node->data  = malloc(data_size);
    new_Sen_node->next = (*head_ref);

    // Copy contents of new_data to newly allocated memory.
    // Assumption: char takes 1 byte.
    int i;
    for (i=0; i<data_size; i++) {
        *(char *)(new_Sen_node->data + i) = *(char *)(new_data + i);
    }

    // Change head pointer as new Sen_node is added at the beginning
    (*head_ref)    = new_Sen_node;
}

// Function to print an integer
void printInt(void *n) {
   printf(" %d", *(int *)n);
}

void printChar(char *n) {
   printf(" %c", *(char *)n);
}

/* Function to print Sen_nodes in a given linked list. fpitr is used
   to access the function to be used for printing current Sen_node data.
   Note that different data types need different specifier in printf() */
void printList(struct Sen_node *Sen_node, void (*fptr)(void *)) {
    while (Sen_node != NULL) {
        (*fptr)(Sen_node->data);
        Sen_node = Sen_node->next;
    }
}

