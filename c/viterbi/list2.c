#include <stdio.h>  
#include <stdlib.h>

typedef struct {
    char** words;
    size_t nWords;
    size_t size;
    size_t block_size;
} word_list;

word_list* create_word_list(size_t block_size) {
    word_list* pWordList = malloc(sizeof(word_list));
    if (NULL != pWordList) {
        pWordList->nWords = 0;
        pWordList->size = block_size;
        pWordList->block_size = block_size;
        pWordList->words = malloc(sizeof(char*)*block_size);
        if (NULL == pWordList->words) {
            free(pWordList);
            return NULL;    
        }
    }
    return pWordList;
}

void delete_word_list(word_list* pWordList) {
    free(pWordList->words);
    free(pWordList);
}

int add_word_to_word_list(word_list* pWordList, char* word) {
    size_t nWords = pWordList->nWords;
    if (nWords >= pWordList->size) {
        size_t newSize = pWordList->size + pWordList->block_size;
        void* newWords = realloc(pWordList->words, sizeof(char*)*newSize); 
        if (NULL == newWords) {
            return 0;
        } else {    
            pWordList->size = newSize;
            pWordList->words = (char**)newWords;
        }

    }

    pWordList->words[nWords] = word;
    ++pWordList->nWords;


    return 1;
}

char** word_list_start(word_list* pWordList) {
        return pWordList->words;
}

char** word_list_end(word_list* pWordList) {
        return &pWordList->words[pWordList->nWords];
}

int main(int argc, char** argv) {

        word_list* myWords = create_word_list(2);
        add_word_to_word_list(myWords, "Hello");
        add_word_to_word_list(myWords, "World");
        add_word_to_word_list(myWords, "Goodbye");

        char** iter;
        for (iter = word_list_start(myWords); iter != word_list_end(myWords); ++iter) {
                printf("%s ", *iter);
        }

        delete_word_list(myWords);

        return 0;
}
