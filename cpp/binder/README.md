# binder demo

Basic demo of [binder](https://github.com/RosettaCommons/binder), a tool to
autogenerate pybind11 bindings for c++ code.

To build and test binder

1. `brew install cmake ninja pybind11`

2. build binder using the `build.py` script in the `binder` repo. Ignore the documentation
   since it has dated installation instructions.

3. copy or move the resulting `build` binder directory to `~/.binder` and
   symlink as follows: `~/.binder/bin/binder` to `/usr/local/bin/binder`

4. run `./build.sh` in this directory and a demo python extension should be in the `build` directory

