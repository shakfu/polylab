"""Common utility functions and classes."""
from pathlib import Path

def is_number(obj):
    """Checks if obj is an or a float or a long.

    >>> is_number(10)
    True
    """
    return isinstance(obj, (int, float))


def is_xlsx(path):
    """Returns true if uri is a ``*.xlsx`` file.

    >>> p = Path('/tmp/hello.xlsx')
    >>> p.touch()
    >>> is_xlsx('/tmp/hello.xlsx')
    True
    >>> p.unlink()
    """
    return all([
        Path(path).is_file(),
        path.endswith('.xlsx')
    ])


def is_yaml(path):
    """Returns true if uri is a ``*.yaml`` or ``*.yml`` file.

    >>> p = Path('/tmp/hello.yaml')
    >>> p.touch()
    >>> is_yaml('/tmp/hello.yaml')
    True
    >>> p.unlink()
    """
    return all([
        Path(path).is_file(),
        any(path.endswith(i) for i in ['.yml', '.yaml'])
    ])

