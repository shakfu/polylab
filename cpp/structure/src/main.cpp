
#include <iostream>
#include "vehicle.h"

using namespace std;

int main() {
    Vehicle v;
    v.cost = 10.2;
    v.weight = 22.3;
    std::cout << v.cost_per_kg() << std::endl;
    std::cout << "hello world!" << std::endl;
    std::cout << v.to_string() << std::endl;
    return 0;
}


