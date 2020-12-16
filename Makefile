# trivial makefile
# following https://nullprogram.com/blog/2017/08/20/
.POSIX:
.SUFFIXES:

CC = gcc
CFLAGS = -W -g -Wall

all: lisp # clean

lisp: lisp.o
	$(CC) $(CFLAGS) lisp.o -o lisp

lisp.o: lisp.c
	$(CC) -c $(CFLAGS) lisp.c

clean:
	rm -f *.o *.out


.SUFFIXES: .c .o

.c.o:
	$(CC) $(CFLAGS) -c $<