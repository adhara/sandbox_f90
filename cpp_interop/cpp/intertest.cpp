#include <iostream>
#include "intertest.h"


using namespace std;

int main (int argc, char **argv)
{
   int a = 5;
   float b;

   f90sub_(&a, &b);

   cout << "b: " << b << endl;
}