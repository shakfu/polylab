#include "vehicle.h"


float Vehicle::cost_per_kg() {
    return cost / weight;
}

string Vehicle::to_string() {
    string str = "<Vehicle cost: " + std::to_string(cost) + ">";
    return str;
}

