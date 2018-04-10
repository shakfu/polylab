#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <dirent.h>
#include <unistd.h>
#include <sys/stat.h>
#include <sys/param.h>

#define FALSE 0
#define TRUE !FALSE

char *DEFAULT_DIR = "/home/sa/src";

//extern int alphasort();

void combine(char *destination, const char *path1, const char *path2)
{
	if (path1 == NULL && path2 == NULL) {
		strcpy(destination, "");;
	} else if (path2 == NULL || strlen(path2) == 0) {
		strcpy(destination, path1);
	} else if (path1 == NULL || strlen(path1) == 0) {
		strcpy(destination, path2);
	} else {
		char directory_separator[] = "/";

		const char *last_char = path1;
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

int check_exists_isdir(char *path)
{
	// check that the directory exists and is a directory
	struct stat sb;

	if (stat(path, &sb) == 0 && S_ISDIR(sb.st_mode)) {
		//printf("SUCCESS: '%s' exists and is a directory\n", path);

		return TRUE;
	} else {
		printf
		    ("FAILURE: '%s' does not exists and/or is not a directory\n",
		     path);
		return FALSE;
	}
}

static int dir_select(const struct dirent *entry)
{
	// check for "." & ".."
	if ((strcmp(entry->d_name, ".") == 0) ||
	    (strcmp(entry->d_name, "..") == 0))
		return FALSE;

	// check that entry is a directory
	if (entry->d_type == DT_DIR)
		return TRUE;
	else
		return FALSE;
}

void update_srcdir(char *path)
{
	// declarations
	int count_i, i, count_j, j;
	struct dirent **files_i;
	struct dirent **files_j;
	char root[MAXPATHLEN];

	// check that the directory exists and is a directory
	check_exists_isdir(path);

	// populate root directory
	strcpy(root, path);

	// topdir level
	count_i = scandir(path, &files_i, dir_select, alphasort);
	if (count_i <= 0) {
		printf("no files in directory %s\n", path);
	}
	// project level
	for (i = 1; i < count_i + 1; ++i) {
		char project[strlen(root) + strlen(files_i[i - 1]->d_name) + 2];
		combine(project, root, files_i[i - 1]->d_name);
		printf("\nproject: %s\n", project);

		// project directory level
		count_j = scandir(project, &files_j, dir_select, alphasort);
		if (count_j <= 0) {
			printf("%d files in directory %s\n", count_j, project);
		}
		//printf("number of files_j = %d\n", count_j);

		for (j = 1; j < count_j + 1; ++j) {
			char result[strlen(project) +
				    strlen(files_j[j - 1]->d_name) + 2];
			combine(result, project, files_j[j - 1]->d_name);
			//printf("\tdir: %s\n", result);
			if (strcmp(files_j[j - 1]->d_name, ".bzr") == 0) {
                chdir(project);
				system("bzr pull");
				system("bzr update");
			}

			else if (strcmp(files_j[j - 1]->d_name, ".hg") == 0) {
				chdir(project);
				system("hg pull");
				system("hg update");
			}

			else if (strcmp(files_j[j - 1]->d_name, ".svn") == 0) {
				chdir(project);
				system("svn update");
			}

			else if (strcmp(files_j[j - 1]->d_name, ".git") == 0) {
				chdir(project);
				system("git pull");
			}

			else
				continue;
		}

	}
	printf("\n");

}

