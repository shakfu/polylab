#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

int main()
{

    char* fileName = "hello.txt";

    int fd = open(fileName, O_RDWR);

    if (fd == -1) {
        printf("\nError Opening File!!\n");
        exit(1);
    } else {
        printf("\nFile %s opened sucessfully!\n", fileName);
    }

    char buffer[1024];

    int bytesRead = read(fd, buffer, sizeof(buffer));

    printf("%d bytes read!\n", bytesRead);
    printf("File Contents: %s\n", buffer);

    return 0;
}
