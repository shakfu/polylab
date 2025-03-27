#include <stdio.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

int main()
{
    pid_t pid;
    int status;

    pid = fork();

    if (pid == -1) {
        perror("fork");
        return 1;
    } else if (pid == 0) {
        // Child process
        char* args[] = { "/bin/ls", "-l", NULL };
        execv(args[0], args);
        perror("execv"); // This line is only reached if execv fails
        return 1;
    } else {
        // Parent process
        waitpid(pid, &status, 0);
        printf("Child process exited with status %d\n", WEXITSTATUS(status));
    }

    return 0;
}
