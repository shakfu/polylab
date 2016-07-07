
typedef struct sPerson
{
    // data
    char *name;
    int age;
    
    // default static methods
    void (*print)(char *txt, char *sub);
    void (*talk) (char *txt);
} *Person;


Person Person_new(char *name, int age) ;

void Person_delete(Person *this);

void Person_show(Person this);

void Person_change(Person this) ;
