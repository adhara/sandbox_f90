CC=g++
FCOMP=gfortran44
RM=rm -f
BIN=teste
INCDIR=cpp
LIBDIR=.
LIBNAME=intertest


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
	$(CC) intertest.o -L$(LIBDIR) -l$(LIBNAME) -o $(BIN)

