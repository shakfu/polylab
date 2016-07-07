import channel

c = channel.BChannel(["hello", "msg b", "world", "nice one"])
c.send()
c.receive()

# c = channel.Channel()
# c.send("LA LA")
# c.receive()
