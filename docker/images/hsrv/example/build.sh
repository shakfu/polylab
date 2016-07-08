ghc --make -o server -O2 -optl-static src/Main.hs
rm ./src/*.hi
rm ./src/*.o
echo "use upx to shrink executable"

