#include <stdio.h>
#include <stdlib.h>
#include <string.h>


typedef char *string; 

string say(string);

int main() {
    string a = NULL;
    //string b = NULL;
    string b;

    a = say("Hi there Chris");
    free(a);

    b = say("Goodbye");
    free(b);
    
    printf("from main: %s %s\n", a, b);
}


string say(string s) {
    string res = (string) malloc(strlen(s)+1);
    strcpy(res, s);
    printf("from say: the string is %s\n", res);
    return res;
}


