double** polygon_create(int n);

void polygon_destroy(int n, double** matrix);

void polygon_angles(int n, double** matrix);

void polygon_print(int n, double** matrix);

double polygon_area(int n, double **matrix);

double** polygon_from_file(int n_points, char* path);

void polygon_area_from_file(int n_points, char* path);