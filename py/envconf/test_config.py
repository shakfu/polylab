import os

def test_environconfig():

    from environconfig import EnvironConfig, StringVar, IntVar

    class Config(EnvironConfig):
        """Database configuration from the environment."""
        HOSTNAME = StringVar(default='localhost')
        PORT = IntVar(default=3306)
        USERNAME = StringVar()

    host = 'WORLD'
    port = 8000

    os.environ['HOSTNAME'] = host
    assert Config.HOSTNAME == host

    os.environ['PORT'] = str(port)
    assert Config.PORT == port

