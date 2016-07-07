#include <stdio.h>
#include <stdlib.h>

struct node {
    int data;
    struct node *next;
};

struct node* add(struct node *head, int data) {
    struct node *tmp;

    if (head == NULL) {
        head=(struct node *)malloc(sizeof(struct node));
        if (head == NULL) {
            printf("Error! memory is not available\n");
            exit(0);
        }
        head-> data = data;
        head-> next = head;
    } else {
        tmp = head;

        while (tmp-> next != head)
            tmp = tmp-> next;
        tmp-> next = (struct node *)malloc(sizeof(struct node));
        if (tmp -> next == NULL)
        {
            printf("Error! memory is not available\n");
            exit(0);
        }
        tmp = tmp-> next;
        tmp-> data = data;
        tmp-> next = head;
    }
    return head;
}

void printlist(struct node *head)
{
    struct node *current;
    current = head;
    if (current!= NULL)
    {
        do
        {
            printf("%d\t",current->data);
            current = current->next;
        } while (current!= head);
        printf("\n");
    }
    else
        printf("The list is empty\n");

}

void destroy(struct node *head)
{
    struct node *current, *tmp;

    current = head->next;
    head->next = NULL;
    while (current != NULL) {
        tmp = current->next;
        free(current);
        current = tmp;
    }
}

int main()
{
    struct node *head = NULL;
    head = add(head,1); /* 1 */
    printlist(head);

    head = add(head,20);/* 20 */
    printlist(head);

    head = add(head,10);/* 1 20 10 */
    printlist(head);

    head = add(head,5); /* 1 20 10 5*/
    printlist(head);

    destroy(head);
    getchar();
}


