/*
 * - header must be void (use arguments for return values)
 * - pointers for each argument (f90 always uses passing by reference)
 */

// avoid name mangling
#ifdef __cplusplus
extern "C" {
#endif

void intertest_f90sub_(int *a, float *b);

#ifdef __cplusplus
}
#endif
