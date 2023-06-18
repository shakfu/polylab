import eg


def test_pair():
    d = eg.getDimensions(10.2, 5.5)

    assert d.first == 10.2
    assert d.second == 5.5

if __name__ == '__main__':
    test_pair()
