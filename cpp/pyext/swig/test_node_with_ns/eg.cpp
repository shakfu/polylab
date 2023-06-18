#include "eg.h"

namespace dialect {

double Node::getHeight(void) {
    return height;
}

void Node::setHeight(double h) {
    height = h;
}

void Node::setWidth(double w) {
    width = w;
}

double Node::getWidth(void) {
    return width;
}

dimensions Node::getDimensions(void) {
    return (dimensions)std::make_pair(height, width);
}

void Node::setDimensions(double h, double w) {
    height = h;
    width = w;
}

}


int main() {
    dialect::Node node;
    node.setHeight(10.0);
    node.setWidth(6.0);
}


// int main() {
//     dimensions d = std::make_pair(10.2, 22.2);
//     std::cout << "d.first: " << d.first << "\n";
// }
