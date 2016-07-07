#include "models.h"
#include "common.h"

Person::Person()
{
    debug("created PersonClass");
}

void Person::say()
{
    debug("Called say");
}


Rectangle::Rectangle(double w, double h) {
    width = w;
    height = h;
    }


double Rectangle::area() {
    return width * height;
};


void Rectangle::show() {
    debug("<Rect w:%f h:%f>", width, height);
};