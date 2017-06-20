import std.parallelism;
import std.range;
 
ulong fib(ulong n)
{
    return n < 2 ? 1 : fib(n - 2) + fib(n - 1);
}
 
void main()
{
    ulong[45] sequence;
 
    foreach (x; parallel(iota(sequence.length), 1))
    {
        sequence[x] = fib(x);
    }
}