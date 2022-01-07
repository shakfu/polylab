#include <string>

using namespace std;

class Vehicle {
    public:
        float cost;
        float weight;
    
        float cost_per_kg();

        string to_string();
};
