#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdarg.h>

// boldened by \x1b[1m
#define ANSI_COLOR_RED     "\x1b[1m\x1b[31m"
#define ANSI_COLOR_GREEN   "\x1b[1m\x1b[32m"
#define ANSI_COLOR_YELLOW  "\x1b[1m\x1b[33m"
#define ANSI_COLOR_BLUE    "\x1b[1m\x1b[34m"
#define ANSI_COLOR_MAGENTA "\x1b[1m\x1b[35m"
#define ANSI_COLOR_CYAN    "\x1b[1m\x1b[36m"
#define ANSI_COLOR_RESET   "\x1b[0m"

char *colored(char *str, char *color) {
    char *buff = malloc(strlen(str)+14);
    if (color == "red") {
        sprintf(buff, "%s%s%s", ANSI_COLOR_RED, str, ANSI_COLOR_RESET);
    } else if (color == "green") {
        sprintf(buff, "%s%s%s", ANSI_COLOR_GREEN, str, ANSI_COLOR_RESET);
    } else if (color == "yellow") {
        sprintf(buff, "%s%s%s", ANSI_COLOR_YELLOW, str, ANSI_COLOR_RESET);
    } else if (color == "blue") {
        sprintf(buff, "%s%s%s", ANSI_COLOR_BLUE, str, ANSI_COLOR_RESET);
    } else if (color == "magenta") {
        sprintf(buff, "%s%s%s", ANSI_COLOR_MAGENTA, str, ANSI_COLOR_RESET);
    } else if (color == "cyan") {
        sprintf(buff, "%s%s%s", ANSI_COLOR_CYAN, str, ANSI_COLOR_RESET);
    } else {
        printf("color: %s not recognized", color);
        exit(0);
    }
    return buff;
}


int main (int argc, char const *argv[]) {
    int i;
    char *colors[] = {"red", "green", "yellow", "blue", "magenta", "cyan"};
    int ncolors = 6;

    for (i=0; i < ncolors; i++) {
        printf("this is %s\n", colored(colors[i], colors[i]));
    }
    //printf("this is cyan: %s\n", colored("Hello there", "cyan"));
    return 0;
}
