#include <iostream>


template<typename T>
T add(T a, T b)
{
    return a + b; 
}

template int add<int>(int, int);
template float add<float>(float, float);
template double add<double>(double, double);

int main() 
{

    std::cout << add(1,2) << '\n';

    std::cout << add(1.0, 2.1) << '\n';
}
