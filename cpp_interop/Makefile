CC=g++
FCOMP=g95
RM=rm -f
BIN=teste
INCDIR=cpp
LIBDIR=.
LIBNAME=intertest

# needs to link with fortran compiler lib if you are using intrinsic routines such as 'write'
GF_LIB_DIR = 

# in case g95 lib dir points to a gcc version previous to the one used in g++, the correct gcc lib path must be set before. Wow, this was creepy to find!!
GCC_LIB_DIR = /usr/lib/gcc/x86_64-redhat-linux/4.1.2/


all: $(BIN)

clean:
	$(RM) $(BIN) *.o *.mod *.a

libintertest.o:
	$(FCOMP) -c f90/libintertest.f90
	
libintertest.a: libintertest.o
	ar rcs libintertest.a libintertest.o

intertest.o:
	$(CC) -c cpp/intertest.cpp -I$(INCDIR) 
	
teste: intertest.o libintertest.a
	$(CC) intertest.o -L$(LIBDIR) -L$(GCC_LIB_DIR) -L$(GF_LIB_DIR) -l$(LIBNAME) -lf95  -o $(BIN)
