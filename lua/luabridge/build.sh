LUA_PREFIX=`brew --prefix lua`

# g++ --std=c++11 -I/usr/local/opt/lua/include/lua -I. -ILuaBridge -llua -o main main.cpp

g++ --std=c++11 -I${LUA_PREFIX}/include/lua -I. -ILuaBridge -llua -o main main.cpp

