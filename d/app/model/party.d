module model.party;

import model.date;
import model.mixins;

import std.stdio;
import std.string;


interface Party
{
    string say();
    void display();
}

class Person : Party
{
    // add mixin
    mixin DemoMixin;

    string name;
    Date dob;

    // constructor
    this(string name, int day, int month, int year)
    {
        this.name = name;
        this.dob = Date(day, month, year);
    }

    string say()
    {
        return this.name;
    }

    void display()
    {
        writeln(this.say(), " was born in ", this.dob);
    }
}

