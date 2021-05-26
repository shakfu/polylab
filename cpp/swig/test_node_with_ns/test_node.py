import eg


def test_node():
    n = eg.Node()
    n.setDimensions(10.2, 5.5)
    d = n.getDimensions()

    assert d.first == 10.2
    assert d.second == 5.5

if __name__ == '__main__':
    test_node()
