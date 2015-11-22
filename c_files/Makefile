CC=gcc
CFLAGS= -Wall

INC= -I ./headers

SRC=$(wildcard *.c)

all: main

main: $(SRC)
	  $(CC) -o out $(CFLAGS) $(INC) $(SRC)

clean:
	  rm -f *.o
