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

int main()
{
    unique_ptr<int> ptr(new int); // applied to a primitive

    auto p = make_unique<Person>("sa", 12);
    auto q = *p; // deref pointer
    q.name = "sam";
    q.show();
    p->show();
    return 0;
}

