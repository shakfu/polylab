class ICommand(object):
    enabled = False
    def execute():
        "execute the command"
        
class View(object):
    def display(self, msg):
        print msg

class Mediator(object):
    cmds = {}    
    def __init__(self, view):
        self.view = view
    
    def book(self):
        self.cmds['book'].enabled = False
        self.cmds['delete'].enabled = True
        self.cmds['search'].enabled = True
        self.view.display("booking...")

    def delete(self):
        self.cmds['book'].enabled = True
        self.cmds['delete'].enabled = False
        self.cmds['search'].enabled = True
        self.view.display("deleting...")

    def search(self):
        self.cmds['book'].enabled = True
        self.cmds['delete'].enabled = True
        self.cmds['search'].enabled = False
        self.view.display("searching...")


class Book(ICommand):
    def __init__(self, mediator):
        self.mediator = mediator
        self.mediator.cmds['book'] = self

    def execute(self):
        self.mediator.book()
        print 'do booking'


class Search(ICommand):
    def __init__(self, mediator):
        self.mediator = mediator
        self.mediator.cmds['search'] = self

    def execute(self):
        self.mediator.search()
        print 'do searching'


class Delete(ICommand):
    def __init__(self, mediator):
        self.mediator = mediator
        self.mediator.cmds['delete'] = self

    def execute(self):
        self.mediator.delete()
        print 'do deleting'


if __name__ == '__main__':
    med = Mediator(View())
    med.cmds['book'] = Book(med)
    med.cmds['search'] = Search(med)
    med.cmds['delete'] = Delete(med)

