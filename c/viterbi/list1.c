#include <stdio.h>
#include <stdlib.h>

typedef struct s_words {
        char* str;
        struct s_words* next;
} words;

words* create_words(char* word) {
        words* newWords = malloc(sizeof(words));
        if (NULL != newWords){
                newWords->str = word;
                newWords->next = NULL;
        }
        return newWords;
}

void delete_words(words* oldWords) {
        if (NULL != oldWords->next) {
                delete_words(oldWords->next);
        }
        free(oldWords);
}

words* add_word(words* wordList, char* word) {
        words* newWords = create_words(word);
        if (NULL != newWords) {
                newWords->next = wordList;
        }
        return newWords;
}

int main(int argc, char** argv) {

        words* myWords = create_words("Hello");
        myWords = add_word(myWords, "World");

        words* iter;
        for (iter = myWords; NULL != iter; iter = iter->next) {
                printf("%s ", iter->str);
        }
        delete_words(myWords);
        return 0;
}