# Embed Lua Example

- assumes `brew install lua`

- see https://chsasank.com/lua-c-wrapping.html

for lua extension test:

```bash
$ lua
Lua 5.2.4  Copyright (C) 1994-2015 Lua.org, PUC-Rio
> mylib = require 'mylib'
> print(mylib.c_swap(2, 4))
4   2
> print(mylib.mysin(2))
0.90929742682568
```
