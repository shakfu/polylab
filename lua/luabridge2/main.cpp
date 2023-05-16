// main.cpp
#include "lua.hpp"
#include <LuaBridge.h>

#include <iostream>

namespace app {

class Person {
private:
    std::string name;
    int age;

public:
    Person(std::string name, int age) {
        this->name = name;
        this->age = age;
    }

    std::string identify() {
        return this->name + " " + std::to_string(this->age);
    }
};

} // end namespace app

using namespace luabridge;


void report_errors(lua_State *L, int status) {
    if (status == 0) {
        return;
    }

    std::cerr << "[LUA ERROR] " << lua_tostring(L, -1) << std::endl;

    // remove error message from Lua state
    lua_pop(L, 1);
}


int main() {
    lua_State* L = luaL_newstate();
    luaL_dofile(L, "cpp_call_lua.lua");
    luaL_openlibs(L);

    LuaRef s = getGlobal(L, "testString");
    LuaRef n = getGlobal(L, "number");

    std::string luaString = s.cast<std::string>();
    int answer = n.cast<int>();
    std::cout << luaString << std::endl;
    std::cout << "And here's our number:" << answer << std::endl;

    getGlobalNamespace(L)
        .beginNamespace("app")
            .beginClass <app::Person> ("Person")
                .addConstructor <void (*) (std::string name, int age)>()
                .addFunction("identify", &app::Person::identify)
            .endClass()
        .endNamespace();

    // load some code from Lua file
    int scriptload_status = luaL_dofile(L, "lua_call_cpp.lua");
    report_errors(L, scriptload_status);

    return 0;
}
