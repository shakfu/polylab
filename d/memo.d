import std.stdio;
import std.functional;

ulong fib(ulong n)
{
    alias mfib = memoize!(fib);
    return n < 2 ? 1 : mfib(n-2) + mfib(n-1);
}

void main()
{
    foreach (x; 1 .. 45)
    {
        writefln("%s", fib(x));
    }
}
