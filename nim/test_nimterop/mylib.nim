

import os

import nimterop/[build, cimport]

const
  base = currentSourcePath.parentDir() / "build"

echo base


static:
  cDebug()


cCompile(base / "mylib.c")

cImport(base / "mylib.h")
