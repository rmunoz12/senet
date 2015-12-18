#include <stdio.h>
#include <stdlib.h>

// Heavily based on
// http://pseudomuto.com/development/2013/05/02/implementing-a-generic-linked-list-in-c/

typedef struct Sen_node {
  void *data;
  struct Sen_node *next;
  struct Sen_node *prev;
} Sen_node;

typedef struct Sen_list {
  int len;
  int data_size;
  Sen_node *head;
  Sen_node *tail;
} Sen_list;


void new_Sen_list(Sen_list *list, int data_size) {
  list->len = 0;
  list->data_size = data_size;
  list->head = list->tail = NULL;
}

/* Function to add a Sen_node at the beginning of Linked List.
   This function expects a pointer to the data to be added
   and size of the data type */
void push(Sen_list *head_ref, void *new_data) {
  // Allocate memory for Sen_node
  Sen_node* new_Sen_node = malloc(sizeof(Sen_node));

  new_Sen_node->data  = malloc(head_ref->data_size);
//  memccpy(new_Sen_node->data, new_data, head_ref->data_size);


  new_Sen_node->next = head_ref->head;
  new_Sen_node->prev = NULL;
  head_ref->head = new_Sen_node;

  if (head_ref->len == 0) {
    head_ref->tail = head_ref->head;
  } else {
    new_Sen_node->next->prev = new_Sen_node;
  }

  // Copy contents of new_data to newly allocated memory.
  // Assumption: char takes 1 byte.
  int i;
  for (i = 0; i < head_ref->data_size; i++) {
    *(char *)(new_Sen_node->data + i) = *(char *)(new_data + i);
  }

  // Change head pointer as new Sen_node is added at the beginning

  head_ref->len++;
}

// Function to print an integer
void printInt(void *n) {
   printf("%d", *(int *)n);
}

void printStr(void *n) {
   printf("%s", *(char **)n);
}

/* Function to print Sen_nodes in a given linked list. fpitr is used
   to access the function to be used for printing current Sen_node data.
   Note that different data types need different specifier in printf() */
void printList(Sen_list *list, void (*fptr)(void *)) {
  int i = 0;
  Sen_node *n = list->tail;
  printf("[");
  while (i < list->len) {
    (*fptr)(n->data);
    n = n->prev;
    ++i;
    if (i < list->len) {
      printf(", ");
    }
  }
  printf("]");
}


