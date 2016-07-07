class CPU(object):
    def freeze(self): pass
    def jump(self): pass
    def execute(self): pass

class Memory(object):
    def load(self): pass

class Harddrive(object): pass

class Computer(object):
    def __init__(self):
        self.cpu = CPU()
        self.memory = Memory()
        self.harddrive = Harddrive()

    def start(self):
        self.cpu.freeze()
        self.memory.load()
        self.cpu.jump()
        self.cpu.execute()

if __name__ == '__main__':
    computer = Computer()
    computer.start()

