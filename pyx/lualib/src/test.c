#include <stdio.h>
#include <lua.h>
#include <lualib.h>
#include <lauxlib.h>
#include <string.h>

/*
gcc -o test test.c -I/home/sa/src/code/sandbox/pyx/lualib2/include/lua5.1 -L/home/sa/src/code/sandbox/pyx/lualib2/lib/lua5.1 -llua5.1 -lm -ldl
*/

int main()
{
    lua_State *L = lua_open();

    if(luaL_loadfile(L,"settings.lua") || lua_pcall(L,0,0,0))
        printf("Error failed to load %s",lua_tostring(L,-1));
    else
    {

        lua_getglobal(L,"screenWidth");
            const int screenWidth = lua_tonumber(L,-1);
            printf("Screen Width = %d \n", screenWidth);

        lua_getglobal(L,"appName");
            const char *appName = luaL_checkstring(L, -1);
            printf("Screen Name = %s \n", appName);

    }

    lua_close(L);

    /* If we got this far, everything worked */
    printf("Success!\n");

    return 0;
}
