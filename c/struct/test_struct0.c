
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct Person {
    const char* name;
    const char* state;
};

struct Person people[] = { { "sam", "sad" }, { "jon", "sad" } };

int count = sizeof(people) / sizeof(struct Person);

int person_cmp(const struct Person* p1, const struct Person* p2)
{
    return strcmp(p1->name, p2->name);
}

void print_person(const struct Person* c)
{
    printf("%s, the %s\n", c->name, c->state);
}

void find_person(const char* name)
{
    struct Person target, *result;
    target.name = name;
    result = bsearch(&target, people, count, sizeof(struct Person),
                     person_cmp);
    if (result)
        print_person(result);
    else
        printf("Couldn't find %s.\n", name);
}

int main(void)
{
    int i;
    for (i = 0; i < count; i++)
        print_person(&people[i]);
    printf("\n");

    qsort(people, count, sizeof(struct Person), person_cmp);

    for (i = 0; i < count; i++)
        print_person(&people[i]);
    printf("\n");

    find_person("sam");
    find_person("jo");
    find_person("jon");

    return 0;
}
