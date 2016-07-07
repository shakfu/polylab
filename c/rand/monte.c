
#include "monte.h"


int randint(int min_n, int max_n)
{
    return rand() % (max_n - min_n + 1) + min_n;
}

float dice(int N, int ndice, int nsix) {
    int M = 0;
    int six, r;
    int i, j;
    float p;
    
    for (i=0; i < N; i++) {
        six = 0;
        for (j=0; j < ndice; j++) {
            r = randint(1, 6);
            if (r == 6) {
                six += 1;
            }
        }
        if (six >= nsix) {
            M += 1;
        }
    }
    p = (float)M / N;
    return p;
}

int main (int argc, char *argv[])
{
    int i;
    
    // seed the pseudo-random generator
    srand(time(NULL));
    
    if (argc < 4) {
        printf("ERROR: Not enough args. Should be %s N ndice nsix\n", argv[0]);
        exit(0);
    }
    
    int N     = atoi(argv[1]);
    int ndice = atoi(argv[2]); 
    int nsix  = atoi(argv[3]);

    printf("%s N=%d ndice=%d nsix=%d \n", argv[0], N, ndice, nsix);
    
    for (i=0; i < 10; i++) {
        printf ("prob(%d sixes in %d dice repeated %d times) = %f\n", 
            nsix, ndice, N, dice(N, ndice, nsix));
    }
    return 0;
}



