LUA_PREFIX=`brew --prefix lua`

g++ --std=c++17 -I${LUA_PREFIX}/include/lua -I. -ILuaBridge -llua -o main main.cpp

