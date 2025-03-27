#define _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>

#include <gsl/gsl_matrix.h>

#include "common.h"

#define COLS 2
#define ROWS 4

#define POINTS 800003


double gsl_polygon_area(int n_corners, gsl_matrix* m)
{
    double area = 0.0;
    int i, j;

    for (i = 0; i < n_corners; i++) {
        j = (i + 1) % n_corners;
        // area += corners[i][0] * corners[j][1];
        area += gsl_matrix_get(m, i, 0) * gsl_matrix_get(m, j, 1);
        // area -= corners[j][0] * corners[i][1];
        area -= gsl_matrix_get(m, j, 0) * gsl_matrix_get(m, i, 1);
    }
    return abs(area) / 2.0;
}

void points_from_file(int rows, char* path)
{
    FILE* file;
    gsl_matrix* m;

    m = gsl_matrix_alloc(rows, COLS);
    file = fopen(path, "r");
    gsl_matrix_fscanf(file, m);
    fclose(file);

    // display it
    printf("matrix m read with %i rows\n", rows);
    // gsl_matrix_fprintf(stdout, m, "%f");
    printf("area: %f\n", gsl_polygon_area(rows, m));

    // release it
    gsl_matrix_free(m);
}

double** polygon_create(int length)
{
    int m = 2;
    int n = length; // two vals per point
    double* values = calloc(m * n, sizeof(double));
    double** rows = malloc(n * sizeof(double*));
    for (int i = 0; i < n; ++i) {
        rows[i] = values + i * m;
    }
    return rows;
}

void polygon_destroy(double** array)
{
    free(*array);
    free(array);
}

void polygon_print(int length, double** matrix)
{
    int i, j;
    for (i = 0; i < length; i++) {
        for (j = 0; j < COLS; j++) {
            printf("[%i][%i] -> %f\n", i, j, matrix[i][j]);
        }
    }
}

void polygon_test(int length)
{
    double x = 0.25;
    int total = (length + 1) + 3 + length + 1;

    double** points = polygon_create(total);

    // init
    points[0][0] = x;
    points[0][1] = x;
    debug("0: (%f, %f)", points[0][0], points[0][1]);

    polygon_print(total, points);
    polygon_destroy(points);
}

double polygon_area(int n_corners, int corners[n_corners][2])
{
    double area = 0.0;
    int i, j;

    for (i = 0; i < n_corners; i++) {
        j = (i + 1) % n_corners;
        area += corners[i][0] * corners[j][1];
        area -= corners[j][0] * corners[i][1];
    }
    return abs(area) / 2.0;
}

double polygon_area2(int n_corners, int points[n_corners][2])
{
    double area = 0.0;

    for (int i = 0; i < n_corners - 1; i++) {
        area += points[i][0] * points[i + 1][1]
            - points[i + 1][0] * points[i][1];
    }
    return abs(area) / 2.0;
}


double area(int corners[ROWS][COLS], int n_corners)
{
    double _area = 0.0;
    int i, j;

    for (i = 0; i < n_corners; i++) {
        j = (i + 1) % n_corners;
        _area += corners[i][0] * corners[j][1];
        _area -= corners[j][0] * corners[i][1];
    }
    return abs(_area) / 2.0;
}

double area2(int m[ROWS][COLS], int n_corners)
{
    double _area = 0.0;
    int i;

    for (i = 0; i < n_corners - 1; i++) {
        _area += m[i][0] * m[i + 1][1] - m[i + 1][0] * m[i][1];
    }
    return abs(_area) / 2.0;
}


void display_int(int matrix[ROWS][COLS])
{
    int i, j;
    for (i = 0; i < ROWS; i++) {
        for (j = 0; j < COLS; j++) {
            printf("[%i][%i] -> %i\n", i, j, matrix[i][j]);
        }
    }
}


void display(double matrix[ROWS][COLS])
{
    int i, j;
    for (i = 0; i < ROWS; i++) {
        for (j = 0; j < COLS; j++) {
            printf("[%i][%i] -> %f\n", i, j, matrix[i][j]);
        }
    }
}

void sum(int matrix[ROWS][COLS])
{
    int i, j;
    int _sum = 0;
    for (i = 0; i < ROWS; i++) {
        for (j = 0; j < COLS; j++) {
            _sum += matrix[i][j];
            printf("[%i][%i] -> sum: %i\n", i, j, _sum);
        }
    }
}


int read_points(void)
{
    FILE* fp;
    char* line = NULL;
    size_t len = 0;
    double x, y;

    fp = fopen("points.txt", "r");
    if (fp == NULL) {
        exit(EXIT_FAILURE);
    }

    int result;
    while (getline(&line, &len, fp) != -1) {
        // printf("%s", line);
        result = sscanf(line, "%lf %lf\n", &x, &y);
        if (result != 2) {
            printf("result: %i\n", result);
            die("unsuccessful sscanf\n");
        }
        // debug("x: %f y: %f", x, y);
    }

    if (line) {
        free(line);
    }

    exit(EXIT_SUCCESS);
}

void test_sscanf()
{
    int result;
    double x, y;
    char* buffer = "547.25 547.25";
    result = sscanf(buffer, "%lf %lf", &x, &y);
    if (result != 2) {
        printf("result: %i\n", result);
        die("unsuccessful sscanf\n");
    }
    printf("x: %f y: %f\n", x, y);
}


int main(int argc, char** argv)
{
    // int i, j;
    // int matrix[ROWS][COLS] = {
    //     2, 4,
    //     3, -8,
    //     1, 2,
    //     2, 4
    // };

    // display_int(matrix);
    // sum(matrix);

    // printf("area: %f\n", area(matrix, ROWS));
    // printf("area2: %f\n", area2(matrix, ROWS));

    // printf("polygon_area: %f\n", polygon_area(ROWS, matrix));
    // printf("polygon_area2: %f\n", polygon_area2(ROWS, matrix));

    read_points();
    // points_from_file(POINTS, "points.txt");
    //  polygon_test(2);
    //  test_sscanf();

    return 0;
}
