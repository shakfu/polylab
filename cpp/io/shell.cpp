#include <stdio.h>
#include <stdlib.h>

main()
{
    char buf [30];

    for (int i=96; i<=99; i++)
    {
        sprintf(buf, "rm %d.log %d.txt", i, i);
        system(buf);
    }

    system("rm 00.log 00.txt");
}


