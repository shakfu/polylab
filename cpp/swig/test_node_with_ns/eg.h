#include <utility>
#include <iostream>
#include <functional>


namespace dialect {

typedef std::pair <double,double> dimensions;


class Node {
    private:
        double height;
        double width;
    public:
        Node() {
            height = 0.0;
            width = 0.0;
        }
        
        void setHeight(double height);
        double getHeight(void);

        void setWidth(double width);     
        double getWidth(void);

        void setDimensions(double height, double width);
        dimensions getDimensions(void);
};

}