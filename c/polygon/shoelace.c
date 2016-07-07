#define _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "common.h"

#define COLS 3
#define N_POINTS 800003


double** polygon_create(int n)
{
    int i, m = COLS;
    double** points = malloc(n * sizeof(double*));

    if (points == NULL) {
        printf("Error: out of Memory.\n");
        exit(EXIT_FAILURE);
    }

    for (i=0; i < n; i++) {
        points[i] = calloc(m, sizeof(double));
        if (points[i] == NULL)
        {
            break;
        }
    }

    if (i != n) {
        printf("Allocation failure: i=%i\n", i);
    }
    return points;
}

void polygon_destroy(int n, double** matrix)
{
    int i;
    if (matrix != NULL) {
        for (i=0; i < n; i++) {
            free(matrix[i]);
            matrix[i] = NULL;
        }
        free(matrix);
        matrix = NULL;
    }
}

// comparison function for qsort
// compares values of third column of matrix
int compare(const void *pa, const void *pb) {
    const double *a = *(const double **)pa;
    const double *b = *(const double **)pb;    
    return (int) (a[2] > b[2]) - (a[2] < b[2]);
}


void polygon_angles(int n, double** matrix)
{
    int i;
    double cx = 0.0;
    double cy = 0.0;
    double dx, dy;

    for (i=0; i < n; i++) {
        cx += matrix[i][0];
        cy += matrix[i][1];
    }

    cx = cx / n;
    cy = cy / n;

    for (i=0; i < n; i++) {
        dx = matrix[i][0] - cx;
        dy = matrix[i][1] - cy;
        // set angle (first convert from radians to degrees)
        matrix[i][2] = (180/M_PI) * fmod((atan2(dy, dx) + 2.0 * M_PI), (2.0 * M_PI));
    }

    // sort the matrix according to angles
    qsort(matrix, n, sizeof(double), compare);
}

void polygon_print(int n, double** matrix)
{
    int i, j;
    for (i=0; i < n; i++) {
        for (j=0; j < COLS; j++) {
            printf("[%i][%i] -> %f\n", i, j, matrix[i][j]);
        }
    }   
}

double polygon_area(int n, double **matrix)
{
    double area = 0.0;
    int i, j;

    for (i=0; i < n; i++) {
        j = (i+1) % n;
        area += matrix[i][0] * matrix[j][1];
        area -= matrix[j][0] * matrix[i][1];
    }
    return abs(area) / 2.0;
}

double** polygon_from_file(int n_points, char* path)
{
    FILE *fp;
    char *line = NULL;
    size_t len = 0;
    double x, y;
    int result;
    int i = 0;
    double** polygon = polygon_create(n_points);

    // read from file
    fp = fopen(path, "r");
    if (fp == NULL) {
        exit(EXIT_FAILURE);
    }

    // read line by line and parse points
    while (getline(&line, &len, fp) != -1) {        
        result = sscanf(line, "%lf %lf\n", &x, &y);
        if (result != 2) {
            printf("result: %i\n", result);
            printf("unsuccessful sscanf\n");
        }
        // create the array
        polygon[i][0] = x;
        polygon[i][1] = y;
        i++;
    }

    if (line) {
        free(line);
    }
    return polygon;
}

double polygon_area_from_file(int n_points, char* path)
{
    double area;
    double** polygon = polygon_from_file(n_points, path);
    polygon_angles(n_points, polygon);
    area = polygon_area(n_points, polygon);
    // polygon_print(n_points, polygon);
    polygon_destroy(n_points, polygon);
    return area;
}

int main(void)
{
    double area = polygon_area_from_file(N_POINTS, "points.txt");
    printf("no of points: %i\n", N_POINTS);
    printf("area: %f\n", area);    
    exit(EXIT_SUCCESS);
}
