
class Switch(object):
    def __init__(self, cmd_up, cmd_down):
        self.cmd_up = cmd_up
        self.cmd_down = cmd_down

    def up(self):
        self.cmd_up.execute()

    def down(self):
        self.cmd_down.execute()


class Light(object):
    def on(self):
        print 'light is on'

    def off(self):
        print 'light is off'


class AbstractCommand(object):
    def __init__(self, light):
        self.light = light

    def execute():
        "execute command"
    
class FlipUpCommand(AbstractCommand):
    def execute(self):
        self.light.on()

class FlipDownCommand(AbstractCommand):
    def execute(self):
        self.light.off()


def switch(cmd):
    lamp = Light()

    dispatch = {
        'ON'  : FlipUpCommand(lamp),
        'OFF' : FlipDownCommand(lamp)
    }

    dispatch[cmd].execute()


def test():
    # by function
    switch("ON")
    switch("OFF")

    # by class
    light = Light()
    sw = Switch(FlipUpCommand(light), FlipDownCommand(light))
    sw.up()
    sw.down()



