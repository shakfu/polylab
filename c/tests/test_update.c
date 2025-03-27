#include <stdio.h>
#include <stdlib.h>
// #include <libgen.h>
#include <dirent.h>
#include <string.h>
#include <sys/stat.h>
#include <unistd.h>
// #include <sys/types.h>
// #include <sys/dir.h>
#include <sys/param.h>

#define FALSE 0
#define TRUE !FALSE

#define MAX_N_CMDS 2
#define BUFFER_SIZE 2048

extern int alphasort();

typedef int bool;
typedef char* string;

char pathname[MAXPATHLEN];
char initial_directory[BUFFER_SIZE] = ".";
const string default_dir = "/home/sa/src";

enum vcstypes { bzr, hg, svn, git };

struct Action {
    string type;
    string vcs_folder;
    string commands[MAX_N_CMDS];
};

struct Action actions[] = { { "bzr", ".bzr", { "bzr pull", "bzr update" } },
                            { "hg", ".hg", { "hg pull", "hg update" } },
                            { "svn", ".svn", { "svn update" } },
                            { "git", ".git", { "git pull" } } };


int count = sizeof(actions) / sizeof(struct Action);


void print_action(const struct Action* action)
{
    int i;
    int cmd_count = sizeof action->commands / sizeof(int);

    printf("size: %d\n", cmd_count);

    printf("type:%s, folder:%s\n", action->type, action->vcs_folder);
    printf("actions:\n");
    for (i = 0; i < cmd_count; i++)
        printf("\t%s\n", action->commands[i]);
    printf("\n");
}

int listfiles(string directory)
{
    DIR* dir;
    struct dirent* ent;
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
    return EXIT_SUCCESS;
}

void combine(char* destination, const char* path1, const char* path2)
{
    if (path1 == NULL && path2 == NULL) {
        strcpy(destination, "");
        ;
    } else if (path2 == NULL || strlen(path2) == 0) {
        strcpy(destination, path1);
    } else if (path1 == NULL || strlen(path1) == 0) {
        strcpy(destination, path2);
    } else {
        char directory_separator[] = "/";

        const char* last_char = path1;
        while (*last_char != '\0')
            last_char++;
        int append_directory_separator = 0;
        if (strcmp(last_char, directory_separator) != 0) {
            append_directory_separator = 1;
        }
        strcpy(destination, path1);
        if (append_directory_separator)
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

int holding(void)
{

    int i;
    for (i = 0; i < count; i++)
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
    string prog = (char*)basename(filename);
    printf("basename: %s\n", prog);

    // print dirname
    string dir = (char*)dirname(filename);
    printf("dirname: %s\n", dir);

    // test combine
    const char* path1 = "/usr/bin";
    const char* path2 = "filename.txt";
    char result[strlen(path1) + strlen(path2) + 2];
    combine(result, path1, path2);
    printf("length of '/usr/bin':%d\n", strlen(path1));
    printf("%s\n", result);

    // test recombine
    recombine(path1, path2);

    return 0;
}

bool check_exists_isdir(string path)
{
    // check that the directory exists and is a directory
    struct stat sb;

    if (stat(path, &sb) == 0 && S_ISDIR(sb.st_mode)) {
        printf("SUCCESS: '%s' exists and is a directory\n", path);

        return TRUE;
    } else {
        printf("FAILURE: '%s' does not exists and/or is not a directory\n",
               path);
        return FALSE;
    }
}

static int dir_select(const struct dirent* entry)
{
    // check for "." & ".."
    if ((strcmp(entry->d_name, ".") == 0)
        || (strcmp(entry->d_name, "..") == 0))
        return FALSE;

    // check that entry is a directory
    if (entry->d_type == DT_DIR)
        return TRUE;
    else
        return FALSE;
}

void update_srcdir(string path)
{
    // check that the directory exists and is a directory
    check_exists_isdir(path);

    // get list of directories from path
    int count_i, i, count_j, j;
    struct dirent** files_i;
    struct dirent** files_j;
    char root[MAXPATHLEN];

    strcpy(root, path);
    // root = dirname(path);
    printf("root:%s\n", root);

    count_i = scandir(path, &files_i, dir_select, alphasort);
    if (count_i <= 0) {
        printf("no files in directory %s\n", path);
    }
    printf("number of files_i = %d\n", count_i);
    for (i = 1; i < count_i + 1; ++i) {
        printf("%s\n", files_i[i - 1]->d_name);
        char project[strlen(root) + strlen(files_i[i - 1]->d_name) + 2];
        combine(project, root, files_i[i - 1]->d_name);
        printf("project: %s\n", project);

        // next level
        count_j = scandir(project, &files_j, dir_select, alphasort);
        if (count_j <= 0) {
            printf("no files in directory %s\n", path);
        }
        printf("number of files_j = %d\n", count_j);

        for (j = 1; j < count_j + 1; ++j) {
            // printf("\t%s\n", files_j[j-1]->d_name);
            char result[strlen(project) + strlen(files_j[j - 1]->d_name) + 2];
            combine(result, project, files_j[j - 1]->d_name);
            printf("\tdir: %s\n", result);
            if (strcmp(files_j[j - 1]->d_name, ".bzr") == 0) {
                chdir(project);
                printf("\ttype: %s", actions[bzr].type);
                system("bzr pull");
                system("bzr update");
            }

            else if (strcmp(files_j[j - 1]->d_name, ".hg") == 0) {
                chdir(project);
                printf("\ttype: %s", actions[hg].type);
                system("hg pull");
                system("hg update");
            }

            else if (strcmp(files_j[j - 1]->d_name, ".svn") == 0) {
                chdir(project);
                printf("\ttype: %s", actions[svn].type);
                system("svn update");
            }

            else if (strcmp(files_j[j - 1]->d_name, ".git") == 0) {
                chdir(project);
                printf("\ttype: %s", actions[git].type);
                system("git pull");
            }

            else
                continue;
        }
    }
    printf("\n");
}


int main(void)
{

    // check default directory
    update_srcdir(default_dir);

    return EXIT_SUCCESS;
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
