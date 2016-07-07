# test it

from attest import Tests, assert_hook
math = Tests()

@math.test
def arithmetics():
    """Ensure that the laws of physics are in check."""
    assert 1 + 1 == 2

@math.test
def multiplication():
    "ensure times table is good"
    assert 2*10 == 20

if __name__ == '__main__':
    math.run()
