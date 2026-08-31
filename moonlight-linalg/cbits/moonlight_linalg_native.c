#include <stddef.h>

extern void dgemm_(char *transa, char *transb, int *m, int *n, int *k,
                   double *alpha, const double *a, int *lda,
                   const double *b, int *ldb, double *beta,
                   double *c, int *ldc);

void moonlight_dgemm_row_major(int left_rows,
                               int right_columns,
                               int inner_dimension,
                               const double *left_row_major,
                               const double *right_row_major,
                               double *output_row_major) {
  char no_transpose = 'N';
  int m = right_columns;
  int n = left_rows;
  int k = inner_dimension;
  int left_as_column_major_leading_dimension = right_columns;
  int right_as_column_major_leading_dimension = inner_dimension;
  int output_leading_dimension = right_columns;
  double alpha = 1.0;
  double beta = 0.0;

  dgemm_(&no_transpose,
         &no_transpose,
         &m,
         &n,
         &k,
         &alpha,
         right_row_major,
         &left_as_column_major_leading_dimension,
         left_row_major,
         &right_as_column_major_leading_dimension,
         &beta,
         output_row_major,
         &output_leading_dimension);
}
