#ifndef MODELS_H
#define MODELS_H

namespace models {

class Person
{
public:
    Person();
    void say();
};

class Rectangle
{
    double width, height;
public:
    Rectangle(double w, double h);
    double area();
    void show();
};

}

#endif // MODELS_H

