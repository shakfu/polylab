#include <stdlib.h>
#include <stdio.h>
//#include <libgen.h>
#include <string.h>
#include <dirent.h>
#include <unistd.h>


typedef char * string;

#define MAX_N_CMDS  2
#define BUFFER_SIZE 2048

char  initial_directory[BUFFER_SIZE] = ".";
string default_dir = "/home/sa/src";

struct Action
{
    string type;
    string vcs_folder;
    string commands[MAX_N_CMDS];
};

struct Action actions[] =
{
    {"bzr", ".bzr", {"bzr pull", "bzr update"}},
    {"hg",  ".hg",  {"hg pull", "hg update"}},
    {"svn", ".svn", {"svn update"}},
    {"git", ".git", {"git pull"}}
};


int count = sizeof (actions) / sizeof (struct Action);


void print_action(const struct Action *action)
{
    int i;
    int cmd_count = sizeof action->commands / sizeof(int);

    printf("size: %d\n", cmd_count);

    printf("type:%s, folder:%s\n", action->type, action->vcs_folder);
    printf("actions:\n");
    for (i=0; i < cmd_count; i++)
        printf("\t%s\n", action->commands[i]);
    printf("\n");
}

int listfiles(string directory)
{
    DIR *dir;
    struct dirent *ent;
    dir = opendir(directory);
    if (dir != NULL) {

        /* print all the files and directories within directory */
        while ((ent = readdir(dir)) != NULL) {
            printf("%s\n", ent->d_name);
        }
        closedir(dir);
    } else {
        /* could not open directory */
        perror("");
        return EXIT_FAILURE;
    }
}

void combine(char* destination, const char* path1, const char* path2)
{
    if(path1 == NULL && path2 == NULL) {
        strcpy(destination, "");;
    }
    else if(path2 == NULL || strlen(path2) == 0) {
        strcpy(destination, path1);
    }
    else if(path1 == NULL || strlen(path1) == 0) {
        strcpy(destination, path2);
    }
    else {
        char directory_separator[] = "/";

        const char *last_char = path1;
        while(*last_char != '\0')
            last_char++;
        int append_directory_separator = 0;
        if(strcmp(last_char, directory_separator) != 0) {
            append_directory_separator = 1;
        }
        strcpy(destination, path1);
        if(append_directory_separator)
            strcat(destination, directory_separator);
        strcat(destination, path2);
    }
}


int recombine(const char* path1, const char* path2)
{
    char result[strlen(path1) + strlen(path2) + 2];
    combine(result, path1, path2);
    printf("%s\n", result);
    return 0;
}


int main(void)
{

    int i;
    for (i=0; i < count; i++)
        print_action(&actions[i]);
    printf("\n");

    // call system
    system("python --version");

    // get current working directory
    getcwd(initial_directory, BUFFER_SIZE);
    printf("cwd: %s\n", initial_directory);

    // change directory
    chdir(default_dir);

    // get current working directory
    getcwd(initial_directory, BUFFER_SIZE);
    printf("cwd: %s\n", initial_directory);

    // list files in directory
    listfiles(default_dir);

    // test path operations
    char filename[] = "/home/a/b/file.py";

    // print basename
    string prog = (char *) basename(filename);
    printf("basename: %s\n", prog);

    // print dirname
    string dir = (char *) dirname(filename);
    printf("dirname: %s\n", dir);

    // test combine
    const char *path1 = "/usr/bin";
    const char *path2 = "filename.txt";
    char result[strlen(path1) + strlen(path2) + 2];
    combine(result, path1, path2);
    printf("length of '/usr/bin':%d\n", strlen(path1));
    printf("%s\n", result);

    // test recombine
    recombine(path1, path2);

    return 0;
}




//~ main(int argc, char** argv)
//~ {
    //~ int i;
//~
    //~ printf("argc = %d\n", argc);
//~
    //~ for (i = 0; i < argc; i++)
    //~ printf("argv[%d] = \"%s\"\n", i, argv[i]);
//~ }

