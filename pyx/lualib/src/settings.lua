#!/usr/bin/env lua

screenWidth = 14
appName = "jackson"
isfun = true

-- script.lua
-- io.write("hello world\n")

function hello(world)
    io.write("hello " .. world .. "!\n")
    -- assert(0>1, "my error message")
    dofile('test.lua')
    -- error("there is nothing", 1)
end

-- hello("world")
