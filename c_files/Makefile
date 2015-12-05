CC=gcc
CFLAGS= -Wall

INC= -I ./headers

SRC=$(wildcard *.c)
SRC:=$(filter-out tictactoe.c, $(SRC))

all: main

main: $(SRC)
	  $(CC) -o out $(CFLAGS) $(INC) $(SRC)

clean:
	  rm -f *.o
