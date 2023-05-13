
-- cbuf: a circular buffer
-- see: inspired by wikipedia entry

BUFLEN = 4


function cbuf(size)
    local self = {
        buf = {},
        size = size,
        current = 0.0,
        end_ = 0,
        start = 0
    }

    local put = function(value)
        self.end_ = self.end_ + 1
        self.buf[self.end_] = value
        self.end_ = self.end_ % self.size
    end

    local get = function()
        self.start = self.start + 1
        self.current = self.buf[self.start]
        self.start = self.start % BUFLEN
        return self.current
    end

    local get_start = function()
        return self.start
    end

    local get_end = function()
        return self.end_
    end

    local dump = function()
        for i=1, self.size do
            print("buf[".. i .. "] = " .. self.buf[i])
        end
        print("start: " .. self.start)
        print("end: " .. self.end_)
        print("current: " .. self.current)
    end

    return {
        put = put,
        get = get,
        get_start = get_start,
        get_end = get_end,
        dump = dump
    }
end


function main()
    local c = cbuf(4)
    local entry = 0.0
    for i=1, 10 do
        entry = entry + i
        c.put(entry)
        current = c.get()
        -- print(i .. " current: " .. current)
        print(i .. " start: " .. c.get_start() .. " current: " .. current)
    end
    -- c.dump()
end
