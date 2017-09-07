module model.animal;

import std.stdio;
import std.string;

class Animal
{
    string name;
    int age = 10;

    // constructor
    this(string name)
    {
        this.name = name;
    }

    override string toString()
    {
        return format("<Animal: %s>", this.name);
    }

    int months() const @property
    {
        return this.age * 12;
    }

    string say()
    {
        return this.name;
    }

    void display()
    {
        writeln(this.say());
    }
}


class Bird : Animal
{
    // constructor
    this(string name)
    {
        super(name);
    }

    override string toString()
    {
        return format("<Bird: %s>", this.name);
    }
}
