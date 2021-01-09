from numpy import array, dot
from scipy.linalg import solve, inv

# Solve the equation a x = b for x.
a = array([[1,1], [2,4]])
b = array([2,7])
x = solve(a, b)

print x

# verify it is correct

# via matrix dot multiplication operation
assert (dot(a, x) == b).all()

#via inverse of square matrix solution
assert ((inv(a) * b).sum(axis=1) == x).all()

