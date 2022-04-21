import nimpy

proc add(x: int, y: int): int {.exportpy.} = 
  return x+y


