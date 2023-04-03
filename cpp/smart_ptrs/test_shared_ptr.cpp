/**
 * @defgroup   unique_ptr demo;
 *
 * @brief      This file implements demo.
 *
 * @author     Sa
 * @date       2023
 */

#include <iostream> // For std::cout
#include <memory> // For std::unique_ptr, std::make_unique

using namespace std; // dropping std::


class Person {
  public:
    int age;
    string name;

    Person(string _name, int _age) {
        age = _age;
        name = _name;
    }

    ~Person() {
        cout << "deleting: " << name << endl;
    }

    void show() {
        cout << "age: " << age << endl;
        cout << "name: " << name << endl;
    }
};

void demo()
{
    // creating shared pointer
    shared_ptr<string> ptr1 = make_shared<string>();
    *ptr1 = "Educative";
    // Only one pointer points to string object
    cout << "ptr1 count = " << ptr1.use_count() << endl;
    // Now second pointer points to the same int object
    shared_ptr<string> ptr2(ptr1);
    cout << "ptr2 is pointing to the same object as ptr1. Now counts are:" << endl;
    // Shows the count after two pointer points to the same object.
    cout << "ptr2 count = " << ptr2.use_count() << endl;
    cout << "ptr1 count = " << ptr1.use_count() << endl;
}

int main()
{
    auto p1 = make_shared<Person>("sa", 12);
    cout << "p1 count: " << p1.use_count() << endl;
    shared_ptr<Person> p2(p1);
    cout << "p2 count: " << p2.use_count() << endl;    
    p1->show();
    p2->show();
    return 0;
}

