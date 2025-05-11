#include <iostream>


template<typename T>
T add(T const a, T const b)
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

class composition
{
public:
    template <typename T = int> // type template parameter can have a default value,

    T add(T const a, T const b)
    {
        return a + b;
    }
};

template <typename T>
class wrapper
{
public:
    wrapper(T const v) : value(v)
    {}

    T const& get() const { return value; }

    template <typename U>
    U as() const
    {
        return static_cast<U>(value);
    }
private:
   T value;
};


template <typename T, size_t S>
class buffer
{
    T data_[S];
public:
    constexpr T const * data() const { return data_; }
    
    constexpr T& operator[](size_t const index)
    {
        return data_[index];
    }
    constexpr T const & operator[](size_t const index) const
    {
        return data_[index];
    }
};




