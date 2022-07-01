
int add(int x, int y)
{
	return x + y;
}


int sum_array(int length, int *array)
{
	int total = 0;

	for (int i = 0; i < length; i++) {
		total += array[i];
	}

	return total;
}