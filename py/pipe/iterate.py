class Iterator:
    def __iter__(self):
        while True:
            yield 1

it = Iterator()
for i in it:
    print i



