#ifdef __cplusplus
  #include "lua.hpp"
#else
  #include "lua.h"
  #include "lualib.h"
  #include "lauxlib.h"
#endif

//so that name mangling doesn't mess up function names
#ifdef __cplusplus
extern "C"{
#endif

static int c_swap (lua_State *L) {
    //check and fetch the arguments
    double arg1 = luaL_checknumber (L, 1);
    double arg2 = luaL_checknumber (L, 2);

    //push the results
    lua_pushnumber(L, arg2);
    lua_pushnumber(L, arg1);

    //return number of results
    return 2;
}

#ifdef __cplusplus
}
#endif


int main(){
    // Create new Lua state and load the lua libraries
    lua_State *L = luaL_newstate();
    luaL_openlibs(L);

    //Expose the c_swap function to the lua environment
    lua_pushcfunction(L, c_swap);
    lua_setglobal(L, "c_swap");

    // Tell Lua to execute a lua command
    luaL_dostring(L, "print(c_swap(4, 5))");
    return 0;
}

