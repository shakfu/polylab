
-- cbuf: a circular buffer
-- see: inspired by wikipedia entry

BUFLEN = 4




cbuf = {a=1}

function cbuf:new(size)
    o = {
        buf = {},
        size = size,
        current = 0.0,
        end_ = 0,
        start = 0
    }
    self.__index = self
    setmetatable(o, self)
    return o
end

function cbuf:put(value)
    self.end_ = self.end_ + 1
    self.buf[self.end_] = value
    self.end_ = self.end_ % self.size
end

function cbuf:get()
    self.start = self.start + 1
    self.current = self.buf[self.start]
    self.start = self.start % BUFLEN
    return self.current
end

function cbuf:get_start()
    return self.start
end

function cbuf:get_end()
    return self.end_
end

function cbuf:dump()
    for i=1, self.size do
        print("buf[".. i .. "] = " .. self.buf[i])
    end
    print("start: " .. self.start)
    print("end: " .. self.end_)
    print("current: " .. self.current)
end


function main()
    local c = cbuf:new(4)
    local entry = 0.0
    for i=1, 10 do
        entry = entry + i
        c:put(entry)
        current = c:get()
        -- print(i .. " current: " .. current)
        print(i .. " start: " .. c:get_start() .. " current: " .. current)
    end
    -- c.dump()
end
