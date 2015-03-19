#include <iostream>
#include "intertest.h"


using std::cout;
using std::endl;


int main (int argc, char **argv)
{
   int a = 5;
   float b;

   // simple test, call f90 subroutine

   intertest_f90sub__(&a, &b);
   cout << "\ntest 1 -  using f90 shell routine to call f90 module routine that calcs sum[i=0..a](i**2):\n" << b << endl << endl;

   // second test, send a multidimensional array to f90 using a flattened form
   
   cout << "test 2 - send a 3D volume to f90 in flattened form\n\n";
   
   int d1, d2, d3, size;
   d1 = d2 = d3 = 3;
   size = d1 * d2 * d3;
   
   int *array = new int[size];
   for (int x = 0; x < d1; x++)
   {
      for(int y = 0; y < d2; y++) 
      {
         for (int z = 0; z < d3; z++)
         {
            // finding index in the flat form does well as an inline function in a real program
            array[(x * d3 * d2) + (y * d3) + z] = 100 * x + 10 * y + z;
         }
      }
   }

   intertest_f90matrixflat__(&array[0], &size);
   cout << "\n3D slices form: \n";
   printMatrixFlat3D(array, d1, d2, d3);

   // third test, receive a matrix from f90 and show that indices are inverted  
   
   int **matrix =  new int*[d1];
   for (int i = 0; i < d1; i++)
   {
      matrix[i] = new int[d2];
      for (int j = 0; j < d2; j++) matrix[i][j] = 0;
   }

   intertest_f90matrix__(&matrix[0][0], &d1, &d2);
   printMatrix2D(matrix, d1, d2);
}


void printMatrix2D(int **m, int d1, int d2)
{
   for (int x = 0; x < d1; x++)
   {
      for(int y = 0; y < d2; y++) 
      {
         cout << m[x][y] << " ";
      }
      cout << endl;
   }
}

void printMatrixFlat3D(int *m, int d1, int d2, int d3)
{
   for (int x = 0; x < d1; x++)
   {
      for(int y = 0; y < d2; y++) 
      {
         for (int z = 0; z < d3; z++)
         {
            cout << m[(x * d3 * d2) + (y * d3) + z] << " ";
         }
         cout << endl;
      }
      cout << endl << endl;
   }
}
