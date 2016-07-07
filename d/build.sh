
TARGETS="memo parallel"

for f in $TARGETS; do
    echo "compiling $f"
    ldc $f.d
    strip $f
done

rm *.o

