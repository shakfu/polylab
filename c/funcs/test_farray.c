
#include <stdio.h>

void printarr(int a[]) {
    int i;
    for(i = 0;i<5;i++) {
        printf(" %d\n",a[i]);
    }
}

int main() {
    int a[5];
    int i;

    for(i = 0;i<5;i++) {
        a[i]=i;
    }
    printarr(a);
}
