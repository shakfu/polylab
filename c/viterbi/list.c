#include <stdio.h>
#include <stdlib.h>

typedef struct s_words {
        char* str;
        struct s_words* next;
} Words;

Words* create_words(char* word) {
        Words* newWords = malloc(sizeof(Words));
        if (NULL != newWords){
                newWords->str = word;
                newWords->next = NULL;
        }
        return newWords;
}

void delete_words(Words* oldWords) {
        if (NULL != oldWords->next) {
                delete_words(oldWords->next);
        }
        free(oldWords);
}

Words* add_word(Words* wordList, char* word) {
        Words* newWords = create_words(word);
        if (NULL != newWords) {
                newWords->next = wordList;
        }
        return newWords;
}

void print_words(Words* words) {
    Words* iter;
    for (iter = words; NULL != iter; iter = iter->next) {
        printf("%s ", iter->str);
    }
    printf("\n");
}

int main(int argc, char** argv) {

    Words* myWords = create_words("Hello");
    myWords = add_word(myWords, "World");

    Words* iter;
    for (iter = myWords; NULL != iter; iter = iter->next) {
            printf("%s ", iter->str);
    }
    delete_words(myWords);
    return 0;
}