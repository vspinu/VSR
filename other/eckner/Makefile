### Makefile for program 'test'

# Set compiler and linker flags
CC = gcc
CFLAGS = -Wall
LDFLAGS = -lm

# Define dependencies
PROGS = test
CLASS = operators.c
CLASS_H = $(SRCS:.c=.h)
SRCS = test.c $(CLASS)
OBJS = $(SRCS:.c=.o)

default : $(PROGS) 

$(PROGS) : depend $(OBJS)
	$(CC) -o $@ $(OBJS) $(LDFLAGS) 


# The dependencies below make use of make's default rules,
# under which a .o automatically depends on its .c and
# the action taken uses the $(CC) and $(CFLAGS) variables.
# These lines describe a few extra dependencies involved.

depend:: Makefile.dependencies $(SRCS) $(HDRS)

Makefile.dependencies:: $(SRCS) $(HDRS)
	$(CC) $(CFLAGS) -MM $(SRCS) > Makefile.dependencies

-include Makefile.dependencies

clean : 
	/bin/rm -f *.o a.out core $(PROGS) Makefile.dependencies

