.SUFFIXES: *.f *.o *.c   # Deativates implicit rules if no suffixes.

# Makefile for hitsmp.f .
# 20210705 gabella, started this file.
#
# From histmp.f comments
# f77 -ohits hitsmp.f hitsaux.f fakehbk.f    or
# gfortran -ohits hitsmp.f hitsaux.f fakehbk.f  on my Fedora 34 Linux.
#

SRCS = hitsmp.f hitsaux.f fakehbk.f
OBJS = ${SRCS:.c=.o}   # Substitution of .o for .c in SRCS.

FC = gfortran  # Or maybe f77, etc.
FFLAGS =

all: hitsmp

main: hitsmp

hitsmp: ${OBJS}
	/usr/bin/echo "Object files: ${OBJS}."
	${FC} ${FFLAGS} -o $@ ${SRCS}

.PHONY: clean
clean:
	rm *.o

# Compilation rules, if you turn off the ones with 
# .SUFFIXES at the top, i.e. no suffixes.


 
