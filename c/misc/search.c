#include "common.h"


int binarysearch(int n, int array[n], int target)
{
    // return position if t in sorted
    // array[0..n-1] or -1 if t is not pressent
    int lower, upper, index;
    lower = 0;
    upper = n - 1;
    while (lower <= upper) {
        index = (lower + upper) / 2;
        if (array[index] < target) {
            lower = index + 1;
        } else if (array[index] == target) {
            return index;
        } else { // array[index] > target
            upper = index - 1;
        }
        return -1;
    }
}

int main()
{
    // must be sorted
    int xs[5] = { 1, 4, 6, 9, 100 };

    debug("target index: %i", binarysearch(5, xs, 6));
}