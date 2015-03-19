/*
 * - header must be void (use arguments for return values)
 * - pointers for each argument (f90 always uses passing by reference)
 */

// avoid name mangling
#ifdef __cplusplus
extern "C" {
#endif

void intertest_f90sub__(int *a, float *b);
void intertest_f90matrixflat__(int *matrix, int *size);
void intertest_f90matrix__(int *matrix, int *d1, int *d2);

#ifdef __cplusplus
}
#endif

void printMatrix2D(int **m, int d1, int d2);
void printMatrixFlat3D(int *m, int d1, int d2, int d3);

