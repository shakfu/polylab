#include <stdio.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

int main()
{
    pid_t pid;

    printf("Before fork: Process ID = %d\n", getpid());

    pid = fork();

    if (pid < 0) {
        perror("fork failed");
        return 1;
    } else if (pid == 0) {
        // Child process
        printf("Child process: Process ID = %d, Parent ID = %d\n", getpid(),
               getppid());
    } else {
        // Parent process
        wait(NULL); // Wait for the child process to finish
        printf("Parent process: Process ID = %d, Child ID = %d\n", getpid(),
               pid);
    }

    return 0;
}
