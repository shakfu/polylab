import std.stdio;
import std.string;

import model.api;
import core.api;

// external c function
extern (C) int add(int x, int y);

void print(T)(T value)
{
    writefln("(%s)", value);
}

void main()
{
    string[string] map;

    string[string] dict = ["hello": "world", "sand": "which"];

    map["Hello"] = "World";

    writeln(map["Hello"]);

    writeln(dict["hello"]);

    writeln("cfunc: ", add(10, 21));

    Date dob = Date(10, 20, 2014);

    print("Hello");
    print(10);
    print(10.1);
    print(dob);

    foreach (key, value; dict) {
        writeln(key, ": ", value);
    }

    Bird bird = new Bird("jane");
    writeln(bird);

    Person person = new Person("sam", 10, 12, 1974);
    person.display();
    person.func();

    writeln("bird is ", bird.months, " months old");

    int first;
    int second;

    distribute(123, first, second);
    writeln("first: ", first, " second: ", second);

}