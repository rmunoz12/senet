#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

#ifndef SEN_LINKED_LIST
#define SEN_LINKED_LIST

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
//  memcpy(new_Sen_node->data, new_data, head_ref->data_size);


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

void printBool(void *b) {
  printf("%s", *(bool *)b ? "true" : "false");
}

void printEmptyList(void *el) {
  printf("[]");
}

void printGroupList(Sen_list *list, char *(*fptr)(void *)) {
  int i = 0;
  Sen_node *n = list->tail;
  printf("[");
    while (i < list->len) {
    printf("%s", (*fptr)(n->data));
    n = n->prev;
    ++i;
    if (i < list->len) {
      printf(", ");
    }
  }
  printf("]");
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

void *list_elem(Sen_list *list, int i) {
  int x = 0;
  Sen_node *n = list->tail;
  while (x < list->len && x != i) {
    n = n-> prev;
    ++x;
  }
  return n->data;
}

void replace(Sen_list *list, void *new_data, int i) {
  Sen_node *new_node = malloc(sizeof(Sen_node));
  new_node->data = malloc(list->data_size);
  memcpy(new_node->data, new_data, list->data_size);

  int x = 0;
  Sen_node *tmp;
  Sen_node *n = list->tail;
  while (x < list->len) {
    if (x == i - 1) {
      tmp = n->prev;
      n->prev = new_node;
      n = tmp;
    } else if (x == i) {
      new_node->prev = n->prev;
      new_node->next = n->next;
      n = n->prev;
    } else if (x == i + 1) {
      tmp = n->prev;
      n->next = new_node;
      n = tmp;
    } else {
      n = n->prev;
    }
    ++x;
  }

  if (list->len > 0) {
    if (i == 0) {
      list->tail = new_node;
    }
    if (i == list->len - 1) {
      list->head = new_node;
    }
  }
}

#endif
