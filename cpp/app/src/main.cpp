// classes example
#include <iostream>
#include <map>
#include <vector>

#include "format.h"
#include "tinyformat.h"
#include "common.h"
#include "mlib.h"

using namespace std;


struct Point {
    string name;
    int x;
    int y;
    void show() {
        cout << TAB << "name: " << name << endl;
        cout << TAB << COLOR_BOLD_RED "x: " COLOR_RESET << x << endl;
        cout << TAB << "y: " << y << endl;
        // fmt::print("Hello, {}!\n", name);
        tfm::printf("name: %s\n", name);
    }
};


class Rectangle {
    double width, height;
    map <string, string> store;
    vector <int> vec;
    Point point  = {"center", 10, 12};
  public:
    Rectangle(double w, double h) {
        width = w;
        height = h;
        store["status"] = "ok";
        vec.push_back(10);
        vec.push_back(20);
    } 
    string status() {
        return store["status"];
    }
    void dump() {
        // lambda function
        auto title = [](string s) {cout << s << ":" << endl;};
        // auto title = [](string s) { printf("%s:", s.c_str()); };
        title("point");
        point.show();
        title("vec");
        for (int &i : vec) {
            cout << TAB << i << endl;
        }
        title("store");
        for (auto &item : store) {
            cout << TAB << item.first << " -> " << item.second << endl;
        }
    }
    double area() {
        return width * height;
}};


int main()
{
	Rectangle rect(3.2, 4.1);
	cout << "area: " << rect.area() << endl;
	cout << "status: " << rect.status() << endl;
	rect.dump();
    debug("add: %i", add(21, 10));
    debug("sub: %i", sub(21, 10));
	return 0;
}
