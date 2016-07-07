module model.date;

import std.string;

struct Date
{
    int day;
    int month;
    int year;

    string toString()
    {
        return format("%s/%s/%s", day, month, year);
    }
}