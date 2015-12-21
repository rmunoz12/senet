#include <stdio.h>
#include <stdlib.h>

#ifndef SEN_READ
#define SEN_READ

char SENET_BUFF[128];

char *_snt_read(int x) {
  // http://stackoverflow.com/questions/4404368/using-scanf-to-read-in-certain-amount-of-characters-in-c
  char *s = malloc(sizeof(char) * 128);
  sprintf(SENET_BUFF, "%%%dc", x);
  scanf(SENET_BUFF, s);
  return s;
}

void _snt_clear_input() {
    int c;
    while ( (c = getchar()) != '\n' && c != EOF ) ;
}

#endif
