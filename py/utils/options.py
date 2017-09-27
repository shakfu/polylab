from easydict import EasyDict


def easy_options(options):
    """Returns dict as an EasyDict instance.

    >>> class A:
    ...     def __init__(self, a=10):
    ...         self.a = a
    ...
    >>> a = A()
    >>> d = {'a':1}
    >>> ez = EasyDict(d)
    >>> easy_options(d)
    {'a': 1}
    >>> ez_ = easy_options(ez)
    >>> ez_ == ez
    True
    >>> easy_options(a)
    {'a': 10}
    >>> easy_options(None)
    >>>
    """
    if isinstance(options, EasyDict):
        return options
    if isinstance(options, dict):
        return EasyDict(options)
    if hasattr(options, '__dict__'):
        return EasyDict(vars(options))
    return options


