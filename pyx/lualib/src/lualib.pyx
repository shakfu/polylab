
# type definitions
ctypedef int size_t
ctypedef char* string
ctypedef double lua_Number
ctypedef int lua_Integer

# compile-time constants
DEF MYCONSTANT = 121232


cdef extern from "lua.h":
    struct lua_State:
        pass
    
    lua_State *lua_open()
    void lua_close (lua_State *L)
    
    # basic stack manipulation
    # -------------------------------------------------------
    int  lua_gettop (lua_State *L)
    void lua_settop (lua_State *L, int idx)
    void lua_pushvalue (lua_State *L, int idx)
    void lua_remove (lua_State *L, int idx)
    void lua_insert (lua_State *L, int idx)
    void lua_replace (lua_State *L, int idx)
    int  lua_checkstack (lua_State *L, int sz)
    void lua_xmove (lua_State * from_, lua_State *to, int n)

    # access functions (stack -> C)
    # -------------------------------------------------------
    int lua_isnumber (lua_State *L, int idx)
    int lua_isstring (lua_State *L, int idx)
    int lua_iscfunction (lua_State *L, int idx)
    int lua_isuserdata (lua_State *L, int idx)
    int lua_type (lua_State *L, int idx)
    string lua_typename (lua_State *L, int tp)

    int lua_equal (lua_State *L, int idx1, int idx2)
    int lua_rawequal (lua_State *L, int idx1, int idx2)
    int lua_lessthan (lua_State *L, int idx1, int idx2)

    lua_Number lua_tonumber (lua_State *L, int idx)
    lua_Integer lua_tointeger (lua_State *L, int idx)
    int lua_toboolean (lua_State *L, int idx)
    string lua_tolstring (lua_State *L, int idx, size_t *len)
    size_t lua_objlen (lua_State *L, int idx)

    # push functions (C -> stack)
    # -------------------------------------------------------
 
    void lua_pushnil (lua_State *L)
    void lua_pushnumber (lua_State *L, lua_Number n)
    void lua_pushinteger (lua_State *L, lua_Integer n)
    void lua_pushlstring (lua_State *L, char *s, size_t l)
    void lua_pushstring (lua_State *L, char *s)
    #string lua_pushvfstring (lua_State *L, char *fmt, va_list argp)
    #string lua_pushfstring (lua_State *L, const char *fmt, ...)
    #void lua_pushcclosure (lua_State *L, lua_CFunction fn, int n)
    void lua_pushboolean (lua_State *L, int b)
    void lua_pushlightuserdata (lua_State *L, void *p)
    int  lua_pushthread (lua_State *L)

    # get functions (Lua -> stack)
    # -------------------------------------------------------
 
    void lua_gettable (lua_State *L, int idx)
    void lua_getfield (lua_State *L, int idx, char *k)
    void lua_rawget (lua_State *L, int idx)
    void lua_rawgeti (lua_State *L, int idx, int n)
    void lua_createtable (lua_State *L, int narr, int nrec)
    #void *(lua_newuserdata) (lua_State *L, size_t sz)
    int  lua_getmetatable (lua_State *L, int objindex)
    void lua_getfenv (lua_State *L, int idx)

    # set functions (stack -> Lua)
    # -------------------------------------------------------
    
    void lua_settable (lua_State *L, int idx)
    void lua_setfield (lua_State *L, int idx, char *k)
    void lua_rawset (lua_State *L, int idx)
    void lua_rawseti (lua_State *L, int idx, int n)
    int  lua_setmetatable (lua_State *L, int objindex)
    int  lua_setfenv (lua_State *L, int idx)

    # `load' and `call' functions (load and run Lua code)
    # -------------------------------------------------------
    
    void lua_call (lua_State *L, int nargs, int nresults)
    int  lua_pcall (lua_State *L, int nargs, int nresults, int errfunc)
    #~ int lua_cpcall (lua_State *L, lua_CFunction func, void *ud)
    #~ int lua_load (lua_State *L, lua_Reader reader, void *dt, char *chunkname)
    #~ int lua_dump (lua_State *L, lua_Writer writer, void *data)

    # coroutine functions
    # -------------------------------------------------------
 
    int lua_yield (lua_State *L, int nresults)
    int lua_resume (lua_State *L, int narg)
    int lua_status (lua_State *L)

    # miscellaneous functions
    # -------------------------------------------------------
    int  lua_error (lua_State *L)
    int  lua_next (lua_State *L, int idx)
    void lua_concat (lua_State *L, int n)

#~ LUA_API lua_Alloc (lua_getallocf) (lua_State *L, void **ud)
#~ LUA_API void lua_setallocf (lua_State *L, lua_Alloc f, void *ud)

    # some useful macros
    # -------------------------------------------------------
    void lua_pop(lua_State *L, int n)
    void lua_newtable(lua_State *L)
#define lua_register(L,n,f) (lua_pushcfunction(L, (f)), lua_setglobal(L, (n)))
#define lua_pushcfunction(L,f)	lua_pushcclosure(L, (f), 0)
    size_t  lua_strlen (lua_State *L, int idx)

    int  lua_isfunction (lua_State *L, int n)
    int  lua_istable(lua_State *L, int n)
    int  lua_islightuserdata(lua_State *L, int n)
    int  lua_isnil(lua_State *L, int n)
    int  lua_isboolean(lua_State *L, int n)
    int  lua_isthread(lua_State *L, int n)
    int  lua_isnone(lua_State *L, int n)
    int  lua_isnoneornil(lua_State *L, int n)
    void lua_pushliteral(lua_State *L, string s)
    void lua_setglobal(lua_State *L, string s)
    void lua_getglobal(lua_State *L, string s)
    string lua_tostring(lua_State *L, int i)

cdef extern from "lualib.h":
    void luaL_openlibs (lua_State *L)

cdef extern from "lauxlib.h":
    lua_State *luaL_newstate()
    int luaL_loadfile(lua_State *L, string filename)
    int luaL_dofile(lua_State *L, string filename)
    int luaL_dostring(lua_State *L, string codestring)
    #int luaL_checkstring(lua_State *L, int idx)

cdef class Application:
    cdef string _name
    cdef lua_State *L
    cdef string lua_file
    cdef list attributes
    
    def __init__(self, string lua_file, list attributes):
        self.lua_file = lua_file
        self.attributes = attributes
        self.L = lua_open()

    def __cinit__(self):
        self._name = ""

    property name:
        "the name of the application"
        def __get__(self):
            return self._name
        def __set__(self, value):
            self._name = value
        def __del__(self):
            self._name = ""
        
    def describe(self):
        error = luaL_loadfile(self.L, self.lua_file) or lua_pcall(self.L, 0,0,0)
        if error:
            print 'ERROR: %s is not loaded' % self.lua_file
        
        for attr in self.attributes:
            value = None
            lua_getglobal(self.L, attr)
            TYPE = lua_type(self.L, -1)
            print 'type:', lua_typename(self.L, TYPE)
            if lua_isnumber(self.L, -1):
                value = lua_tonumber(self.L, -1)
            elif lua_isstring(self.L, -1):
                value = lua_tostring(self.L, -1)
            elif lua_isboolean(self.L, -1):
                value = bool(lua_toboolean(self.L, -1))
            print attr,':', value
        lua_close(self.L)
        print 'OK'


def execute():
    cdef lua_State *L
    L = lua_open()
    error = luaL_loadfile(L,"settings.lua") or lua_pcall(L, 0,0,0)
    print 'error:', error
    lua_getglobal(L,"screenWidth")
    screenWidth = lua_tonumber(L, -1)
    print type(screenWidth)
    print screenWidth
    print 'ok_execute'
    lua_close(L)


def lua(file=None, code=None):
    "run lua file or code"
    cdef lua_State *L
    L = luaL_newstate()
    luaL_openlibs(L)
    if file:
        luaL_dofile(L, file)
    elif code:
        luaL_dostring(L, code)
    else:
        print '''the 'lua' function can be used as follows:
        lua(file='script.lua')
            or
        lua(code='print(1+1)')
        '''

