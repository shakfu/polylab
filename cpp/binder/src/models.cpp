#include "models.h"
#include <stdio.h>

namespace models {

Person::Person()
{
    printf("created PersonClass\n");
}

void Person::say()
{
    printf("Called say\n");
}


Rectangle::Rectangle(double w, double h) {
    width = w;
    height = h;
    }


double Rectangle::area() {
    return width * height;
};


void Rectangle::show() {
    printf("<Rect w:%f h:%f>\n", width, height);
};

} // namespace models