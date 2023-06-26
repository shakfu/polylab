LLVM=`brew --prefix llvm`
export PATH=${LLVM}/bin:${PATH}
#export PREFIX=${HOME}/.opt/fausti # doesn't work just use default /usr/local
make world
make install
