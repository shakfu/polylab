#include "eg.h"

namespace dialect {

dimensions getDimensions(double x, double y) {
    return (dimensions)std::make_pair(x, y);
}

}



// int main() {
//     dimensions d = std::make_pair(10.2, 22.2);
//     std::cout << "d.first: " << d.first << "\n";
// }
