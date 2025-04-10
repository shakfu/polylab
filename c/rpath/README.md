# setting rpath for macos

from:
- <https://stackoverflow.com/questions/78381702/dynamic-linker-in-mac-is-not-reading-rpath>
- <https://stackoverflow.com/questions/66268814/dyld-library-not-loaded-how-to-correctly-tell-gcc-compiler-where-to-find/66284977#66284977>



## 1. The issue

Let's start with a minimal setup consisting of a main binary and a library, like so:

main.c:
```c
#include <stdio.h>

extern int f(void);

int main(void)
{
    printf("%u\n", f());
    return 0;
}
```
xyz.c:

```c
int f(void)
{
    return 42;
}
```

command line:

```sh
% cc -Wall -O3 -shared -o libxyz.dylib xyz.c
% cc -Wall -O3 -o main main.c -L. -lxyz
```

This works. You can run `./main` and it will print `42`.
However, if you now create a folder `lib`, move `libxyz.dylib` there and recompile `main` like so:

```sh
% cc -Wall -O3 -o main main.c -Llib -lxyz
```

Then the compilation will still succeed, however launching it will not:

```sh
% ./main
dyld: Library not loaded: libxyz.dylib
  Referenced from: /private/tmp/./main
  Reason: image not found
```

But if you go back and recompile libxyz.dylib to the lib folder directly and then rebuild main, like so:

```sh
% cc -Wall -O3 -shared -o lib/libxyz.dylib xyz.c
% cc -Wall -O3 -o main main.c -Llib -lxyz
```

Then it will once again work. But just to illustrate, this is the error you get if you move `libxyz.dylib` once more:

```sh
% ./main
dyld: Library not loaded: lib/libxyz.dylib
  Referenced from: /private/tmp/./main
  Reason: image not found
```

Just to make things worse though, you can also produce this error without even moving the library: simply `cd lib` and invoke `../main`.

Also note the difference to before, `libxyz.dylib` vs `lib/libxyz.dylib`. This brings us to the core of the issue.

## 2. The reason

On macOS, each shared library has an "install name", i.e. the path at which it is expected to be found at runtime. This path can take three forms:

- Absolute, e.g. `/usr/lib/libxyz.dylib`.

- Relative, e.g. `lib/libxyz.dylib`.

- Magic, e.g. `@rpath/libxyz.dylib`.

This path is embedded in the Mach-O header of the library, via the `LC_ID_DYLIB` load command. It can be viewed with `otool` like so:

```sh
% otool -l /tmp/lib/libxyz.dylib | fgrep -B1 -A5 LC_ID_DYLIB
Load command 2
          cmd LC_ID_DYLIB
      cmdsize 48
         name lib/libxyz.dylib (offset 24)
   time stamp 1 Thu Jan  1 01:00:01 1970
      current version 0.0.0
compatibility version 0.0.0
```

This load command is created by the linker, whose man page (`man ld`) tells us the following:

```sh
-install_name name
        Sets an internal "install path" (LC_ID_DYLIB) in a dynamic library.
        Any clients linked against the library will record that path as the
        way dyld should locate this library. If this option is not specified,
        then the -o path will be used. This option is also called
        -dylib_install_name for compatibility.
```

This tells us the three steps of how install names work:

1. The linker embeds the name when the library is built.

2. The linker copies the name into binaries linking against that library when those are built.

3. Dyld uses that name to try and load the library.

This will obviously cause issues if libraries are moved, or aren't even being compiled with the install name matching the path at which they will end up.

## 3. The solution

The solution is to change the install name path. Where and how depends on your setup. You can change it by two means:

1. Recompile the library with the correct install name (either `-Wl,-install_name,...` or outright `-o ...`), then recompile the main binary to link against that.

2. Use `install_name_tool`. This is a bit more involved.

In either case, you need to decide what form of install name you want to use:

- **Absolute**. This is recommended for libraries in global paths, shared by all users. You can also use this to point to your user directory, but it's a bit ugly since you can't move the binaries around or distribute them to someone else.

- **Relative**. Being relative to your working directory means this is entirely unreliable. Never use this. Just don't.

- **Magic**. There are three "special" tokens that go beyond absolute and relative paths:

	- `@executable_path` is the runtime directory of the main binary of the process. This is the simplest form, but only works if your libraries are only used in a single main binary.

	- `@loader_path` is the runtime directory of the binary depending on the library. I recommend not using this, as it breaks if you have two binaries in different folders that want to link to the same library.

	- `@rpath` is a list of runtime directories assembled from `LC_RPATH` load commands. This is a bit more complex, but it's the most flexible solution, since it can itself contain `@executable_path` and `@loader_path`.

Use of those allows you to build binaries that can be moved around freely, so long as they all retain their relative position.

For a full description of them, see `man dyld`.

With that out of the way, let's look at implementing the possible solutions. We have:

- `cc -Wl,-install_name,...` to specify an install name at compile time.

- `install_name_tool -id ...` to change the path embedded in a library.

- `install_name_tool -change old new` to change the path embedded in a binary linking against a library.

### 3.1 Absolute paths

If you can recompile both the library and the main binary:

```sh
% cc -Wall -O3 -shared -o /tmp/lib/libxyz.dylib xyz.c
% cc -Wall -O3 -o main main.c -L/tmp/lib -lxyz
```

If you can only recompile the main binary:

```sh
% install_name_tool -id '/tmp/lib/libxyz.dylib' /tmp/lib/libxyz.dylib
% cc -Wall -O3 -o main main.c -L/tmp/lib -lxyz
```

If you cannot recompile either:

```sh
% install_name_tool -id '/tmp/lib/libxyz.dylib' /tmp/lib/libxyz.dylib
% install_name_tool -change 'libxyz.dylib' '/tmp/lib/libxyz.dylib' main
```

### 3.2 @executable_path

If you can recompile both the library and the main binary:

```sh
% cc -Wall -O3 -shared -o lib/libxyz.dylib xyz.c -Wl,-install_name,'@executable_path/lib/libxyz.dylib'
% cc -Wall -O3 -o main main.c -Llib -lxyz
```

If you can only recompile the main binary:

```sh
% install_name_tool -id '@executable_path/lib/libxyz.dylib' lib/libxyz.dylib
% cc -Wall -O3 -o main main.c -Llib -lxyz
```

If you cannot recompile either:

```sh
% install_name_tool -id '@executable_path/lib/libxyz.dylib' lib/libxyz.dylib
% install_name_tool -change 'libxyz.dylib' '@executable_path/lib/libxyz.dylib' main
```

### 3.3 @rpath

Rpath requires manual addition of runtime paths, which requires some planning. Suppose you have the follwing file hierarchy:

```text
a
bin/
	b
libx.dylib
lib/
	liby.dylib
	libz.dylib
```

`a` and `b` are binaries that both link against `libx` and `liby`, which in turn both link against libz. For the install name of `libz`, you can use neither `@executable_path` (because `a` and `b` are in different directories) nor `@loader_path` (because `libx` and `liby` are in different directories). But you can use either of them inside `@rpath`, and here is the decision you have to make:

You can either embed an rpath of `@executable_path` in `a` and `@executable_path/..` in `b`. Then you can use `@rpath` to refer to the project root from all binaries. `libz` would have an install name of `@rpath/lib/libz.dylib`.

Or you can embed an rpath of `@loader_path/lib` in `libx` and `@loader_path` in `liby`. Then you can use `@rpath` to refer to the directory containing each binary. `libz` would have an install name of `@rpath/libz.dylib`.

I generally find the former to be easier to deal with, but the latter may be preferable if you have a large number of binaries scattered over many directories and only a few libraries.

To actually add an `rpath` to a binary, you can use:

- `cc -Wl,-rpath,...` at compile time.

- `install_name_tool -add_rpath ...` afterwards.

So if you can recompile both the library and the main binary:

```sh
% cc -Wall -O3 -shared -o lib/libxyz.dylib xyz.c -Wl,-install_name,'@rpath/lib/libxyz.dylib'
% cc -Wall -O3 -o main main.c -Llib -lxyz -Wl,-rpath,'@executable_path'
```

If you can only recompile the main binary:

```sh
% install_name_tool -id '@rpath/lib/libxyz.dylib' lib/libxyz.dylib
% cc -Wall -O3 -o main main.c -Llib -lxyz -Wl,-rpath,'@executable_path'
```

If you cannot recompile either:

```sh
% install_name_tool -id '@rpath/lib/libxyz.dylib' lib/libxyz.dylib
% install_name_tool -change 'libxyz.dylib' '@rpath/lib/libxyz.dylib' main
% install_name_tool -add_rpath '@executable_path' main
```

Note that if any of your binaries are signed, this will of course invalidate that signature. Use codesign -f ... to replace the existing signature(s).

