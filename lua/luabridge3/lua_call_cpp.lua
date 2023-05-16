
local p = app.Person("sam", 21)
local s = p:identify()
assert(s == "sam 21")
print(s)

