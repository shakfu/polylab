#include <glib.h>
#include <stdio.h>


void test1()
{
    GList* list = NULL;
    list = g_list_append(list, "Austin ");
    printf("The first item is '%s'\n", (char*)list->data);
    list = g_list_insert(list, "Baltimore ", 1);
    printf("The second item is '%s'\n", (char*)g_list_next(list)->data);
    list = g_list_remove(list, "Baltimore ");
    printf("After removal of 'Baltimore', the list length is %d\n", g_list_length(list));
    GList* other_list = g_list_append(NULL, "Baltimore ");
    list = g_list_concat(list, other_list);
    printf("After concatenation: ");
    g_list_foreach(list, (GFunc)printf, NULL);
    list = g_list_reverse(list);
    printf("\nAfter reversal: ");
    g_list_foreach(list, (GFunc)printf, NULL);
    g_list_free(list);
    printf("\n\n");
}

void test2()
{
    GList* list = g_list_append(NULL, "Austin ");
    list = g_list_append(list, "Bowie ");
    list = g_list_append(list, "Charleston ");
    printf("Here's the list: ");
    g_list_foreach(list, (GFunc)printf, NULL);
    GList* last = g_list_last(list);
    printf("\nThe first item (using g_list_first) is '%s'\n", (char*)g_list_first(last)->data);
    printf("The next-to-last item is '%s'\n", (char*)g_list_previous(last)->data);
    printf("The next-to-last item is '%s'\n", (char*)g_list_nth_prev(last, 1)->data);
    g_list_free(list);
    printf("\n\n");
}

void test3()
{
    GList* list = g_list_append(NULL, "Austin ");
    list = g_list_append(list, "Bowie ");
    list = g_list_append(list, "Chicago ");
    printf("Here's the list: ");
    g_list_foreach(list, (GFunc)printf, NULL);
    GList* bowie = g_list_nth(list, 1);
    list = g_list_remove_link(list, bowie);
    g_list_free_1(bowie);
    printf("\nHere's the list after the remove_link call: ");
    g_list_foreach(list, (GFunc)printf, NULL);
    list = g_list_delete_link(list, g_list_nth(list, 1));
    printf("\nHere's the list after the delete_link call: ");
    g_list_foreach(list, (GFunc)printf, NULL);
    g_list_free(list);
    printf("\n\n");
}

void test4()
{
    GList* list = g_list_append(NULL, "Austin ");
    list = g_list_append(list, "Bowie ");
    list = g_list_append(list, "Bowie ");
    list = g_list_append(list, "Cheyenne ");
    printf("Here's the list: ");
    g_list_foreach(list, (GFunc)printf, NULL);
    printf("\nItem 'Bowie' is located at index %d\n", g_list_index(list, "Bowie "));
    printf("Item 'Dallas' is located at index %d\n", g_list_index(list, "Dallas"));
    GList* last = g_list_last(list);
    printf("Item 'Cheyenne' is located at index %d\n", g_list_position(list, last));
    g_list_free(list);
    printf("\n\n");
}

void test5()
{
    GHashTable* hash = g_hash_table_new(g_str_hash, g_str_equal);
    g_hash_table_insert(hash, "Virginia", "Richmond");
    g_hash_table_insert(hash, "Texas", "Austin");
    g_hash_table_insert(hash, "Ohio", "Columbus");
    printf("There are %d keys in the hash\n", g_hash_table_size(hash));
    printf("The capital of Texas is %s\n", (char*)g_hash_table_lookup(hash, "Texas"));
    gboolean found = g_hash_table_remove(hash, "Virginia");
    printf("The value 'Virginia' was %sfound and removed\n", found ? "" : "not ");
    g_hash_table_destroy(hash);
}

int main(int argc, char** argv)
{
    // double linked lists
    test1();
    test2();
    test3();
    test4();

    // hash tables
    test5();

    return 0;
}
