
mkdir -p bind

binder \
    --root-module demo \
    --prefix $PWD/bind \
    --bind models \
    all_includes.hpp \
    -- -std=c++14 -I$PWD/include -DNDEBUG 

echo "building via cmake"
mkdir -p build
cd build
cmake ..
make

