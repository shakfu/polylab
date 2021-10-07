
for f in $(ls tests); do
    echo tests/$f
    ./luajit tests/$f
    echo
done
