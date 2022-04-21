
proc add(x: int, y: int): int {. exportc, dynlib .} =
  result = x + y

