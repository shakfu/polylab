proc add(x: int, y: int): int =
    return x+y


# This is a comment
echo "What's your name? "
var name: string = readLine(stdin)
echo "Hi, ", name, "!"
echo "2+2: ", add(2,2)
