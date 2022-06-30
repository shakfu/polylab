 {.deadCodeElim: on.}
when defined(windows):
  const
    calcdll* = "calc.dll"
elif defined(macosx):
  const
    calcdll* = "libcalc.dylib"
else:
  const
    calcdll* = "libcalc.so"
const
  MY_CONSTANT* = 10

proc add*(x: cint; y: cint): cint {.cdecl, importc: "add", dynlib: calcdll.}
proc sumArray*(length: cint; array: ptr cint): cint {.cdecl, importc: "sum_array",
    dynlib: calcdll.}