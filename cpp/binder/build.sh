
NAME=demo
ROOT=`pwd`
ALL_INCLUDE=all_includes.hpp


binder \
    --root-module demo \
    --prefix $ROOT \
    --bind demo \
    $ALL_INCLUDE \
    -- -std=c++14 -I$ROOT/include -DNDEBUG 


# $PWD/../../build/llvm-6.0.1/build_6.0.1.macos.vole.local.release/bin/binder \
#   --root-module test_struct \
#   --prefix $PWD/bash_bindings/ \
#   --bind testers \
#   all_bash_includes.hpp \
#   -- -std=c++11 -I$PWD/include \
#   -DNDEBUG