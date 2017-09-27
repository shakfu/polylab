
def depends(dikt):
    """Dependency resolver.

    :param dikt: A dependency dictionary in which the values are
                 the dependencies of their respective keys.
    :type dikt: dict

    :return: A list of sets which show the order in which "tasks" can
             be "done" and Groups tasks that can be done simultaneously.

    >>> dependencies = depends(dict(
    ...     a=('b','c'),
    ...     b=('c','d'),
    ...     e=(),
    ...     f=('c','e'),
    ...     g=('h','f'),
    ...     i=('f',)))
    >>> # [set(['h', 'c', 'e', 'd']), set(['b', 'f']), set(['a', 'i', 'g'])]
    >>> len(dependencies)
    3
    """
    deps = dict((key, set(dikt[key])) for key in dikt)
    results = []
    while deps:
        # values not in keys (items without dep)
        dset = set(i for v in list(deps.values()) for i in v) - set(deps.keys())
        # and keys without value (items without dep)
        dset.update(k for k, v in list(deps.items()) if not v)
        # can be done right away
        results.append(dset)
        # and cleaned up
        deps = dict(((k, v - dset) for k, v in list(deps.items()) if v))
    return results
