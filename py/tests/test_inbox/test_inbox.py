from inbox import Inbox

inbox = Inbox()

inbox.counter = 0

@inbox.collate
def handle(to, sender, body):
    #print to, sender, body
    inbox.counter += 1
    print 'email:', inbox.counter

inbox.serve(address='127.0.0.1', port=4467)

