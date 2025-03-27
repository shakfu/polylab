#include <stdio.h>
#include <stdlib.h>

#include <gsl/gsl_linalg.h>
#include <gsl/gsl_matrix.h>
#include <gsl/gsl_sf_bessel.h>
#include <gsl/gsl_statistics.h>
#include <gsl/gsl_vector.h>

#define ARRAY_SIZE(array) sizeof(array) / sizeof(array[0])


void test_matrix_from_file(int rows, int cols, char* path)
{
    FILE* file;
    gsl_matrix* m;

    m = gsl_matrix_alloc(rows, cols);
    file = fopen(path, "r");
    gsl_matrix_fscanf(file, m);
    fclose(file);

    // display it
    printf("matrix m:\n");
    gsl_matrix_fprintf(stdout, m, "%f");

    // release it
    gsl_matrix_free(m);
}

void test_gsl()
{
    double x = 5.0;
    double y = gsl_sf_bessel_J0(x);
    printf("J0(%g) = %.18e\n", x, y);
}

void test_matrix()
{
    gsl_matrix* m = gsl_matrix_alloc(10, 3);

    for (int i = 0; i < 10; i++)
        for (int j = 0; j < 3; j++)
            gsl_matrix_set(m, i, j, 0.23 + 100 * i + j);

    for (int i = 0; i < 10; i++) /* OUT OF RANGE ERROR */
        for (int j = 0; j < 3; j++)
            printf("m(%d,%d) = %g\n", i, j, gsl_matrix_get(m, i, j));

    gsl_matrix_free(m);
}

int test_matrix_to_file()
{
    int i, j, k = 0;
    gsl_matrix* m = gsl_matrix_alloc(100, 100);
    gsl_matrix* a = gsl_matrix_alloc(100, 100);

    for (i = 0; i < 100; i++)
        for (j = 0; j < 100; j++)
            gsl_matrix_set(m, i, j, 0.23 + i + j);

    {
        FILE* f = fopen("test.dat", "wb");
        gsl_matrix_fwrite(f, m);
        fclose(f);
    }

    {
        FILE* f = fopen("test.dat", "rb");
        gsl_matrix_fread(f, a);
        fclose(f);
    }

    for (i = 0; i < 100; i++)
        for (j = 0; j < 100; j++) {
            double mij = gsl_matrix_get(m, i, j);
            double aij = gsl_matrix_get(a, i, j);
            if (mij != aij)
                k++;
        }

    gsl_matrix_free(m);
    gsl_matrix_free(a);

    printf("differences = %d (should be zero)\n", k);
    return (k > 0);
}

void test_vector()
{
    gsl_vector* v = gsl_vector_alloc(3);

    for (int i = 0; i < 3; i++) {
        gsl_vector_set(v, i, 1.23 + i);
    }

    for (int i = 0; i < 3; i++) { /* OUT OF RANGE ERROR */
        printf("v_%d = %g\n", i, gsl_vector_get(v, i));
    }

    gsl_vector_free(v);
}

void test_vector_to_file()
{
    gsl_vector* v = gsl_vector_alloc(100);

    for (int i = 0; i < 100; i++) {
        gsl_vector_set(v, i, 1.23 + i);
    }

    {
        FILE* f = fopen("test.dat", "w");
        gsl_vector_fprintf(f, v, "%.5g");
        fclose(f);
    }

    gsl_vector_free(v);
}

void test_vector_from_file()
{
    gsl_vector* v = gsl_vector_alloc(10);

    {
        FILE* f = fopen("test.dat", "r");
        gsl_vector_fscanf(f, v);
        fclose(f);
    }

    for (int i = 0; i < 10; i++) {
        printf("%g\n", gsl_vector_get(v, i));
    }

    gsl_vector_free(v);
}

void test_linalg()
/* solves the linear system Ax = b
 *
 * (0.18, 0.60, 0.57, 0.96,    (x0,    (1.0,
 *  0.41, 0.24, 0.99, 0.58,     x1,  =  2.0,
 *  0.14, 0.30, 0.97, 0.66,     x2,     3.0,
 *  0.51, 0.13, 0.19, 0.85)     x3)     4.0)
 */
{
    double a_data[] = { 0.18, 0.60, 0.57, 0.96, 0.41, 0.24, 0.99, 0.58,
                        0.14, 0.30, 0.97, 0.66, 0.51, 0.13, 0.19, 0.85 };

    double b_data[] = { 1.0, 2.0, 3.0, 4.0 };

    gsl_matrix_view m = gsl_matrix_view_array(a_data, 4, 4);
    gsl_vector_view b = gsl_vector_view_array(b_data, 4);
    gsl_vector* x = gsl_vector_alloc(4);

    int s;

    gsl_permutation* p = gsl_permutation_alloc(4);
    gsl_linalg_LU_decomp(&m.matrix, p, &s);
    gsl_linalg_LU_solve(&m.matrix, p, &b.vector, x);

    printf("x = \n");
    gsl_vector_fprintf(stdout, x, "%g");
    gsl_permutation_free(p);
    gsl_vector_free(x);
}

void test_stats()
{
    double data[5] = { 17.2, 18.1, 16.5, 18.3, 12.6 };
    double mean, variance, largest, smallest;
    size_t stride = 1;
    size_t size = ARRAY_SIZE(data);

    mean = gsl_stats_mean(data, stride, size);
    variance = gsl_stats_variance(data, stride, size);
    largest = gsl_stats_max(data, stride, size);
    smallest = gsl_stats_min(data, stride, size);

    printf("The dataset is %g, %g, %g, %g, %g\n", data[0], data[1], data[2],
           data[3], data[4]);
    printf("The sample mean is %g\n", mean);
    printf("The estimated variance is %g\n", variance);
    printf("The largest value is %g\n", largest);
    printf("The smallest value is %g\n", smallest);
}

int main(int argc, char** argv)
{
    test_gsl();

    // vectors
    test_vector();
    test_vector_to_file();
    test_vector_from_file();

    // matrices
    test_matrix();
    test_matrix_to_file();
    test_matrix_from_file(4, 3, "matrix.txt");

    // linalg
    test_linalg();

    // stats
    test_stats();

    return 0;
}
