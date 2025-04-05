#include <fcntl.h>
#include <malloc.h>
#include <stdio.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

void my_read(char* arg1)
{
    int fd;
    //  char buffer[100];//don't use static array bcz we don't know how much
    //  data is there in file, create it dynamically
    int rd, n;


    fd = open(arg1, O_RDONLY);
    if (fd == -1) {
        perror("open");
        return;
    } else {
        // first find the size of file .. use stat() system call
        struct stat v;
        stat(arg1, &v);       // storing all file related information into v
        int size = v.st_size; // st.szie is a member of stat structure and it
                              // holds size of file
        // create dynamic array equal to file size
        char* p = malloc(size * sizeof(char));

        printf("enter no of bytes you want to read :\n");
        scanf("%d", &n);

        if (n == 0) {
            // read data from file and copy into dynamic array and print
            int ret = read(fd, p, size);
            if (ret == -1) {
                perror("read");
                return;
            } else {
                printf("data readed from file is :\n");
                printf("%s\n", p);
            }

        } else {
            // read data from file and copy into dynamic array and print
            int ret = read(fd, p, n);
            if (ret == -1) {
                perror("read");
                return;
            } else {
                printf("data readed from file is :\n");
                printf("%s\n", p);
            }
        }

        if (close(fd) < 0) {
            perror("c1");
            return;
        }
    }
}

int main(int argc, char* argv[])
{
        char f_name[100];
        printf("enter the file name :\n");
        scanf("%s", f_name);
        my_read(f_name);
}
