class ICoffeeOrder(object):
    def serve_coffee(context):
        "serve coffee context"


class CoffeeFlavor(ICoffeeOrder):
    def __init__(self, flavor):
        self.flavor = flavor

    def serve_coffee(self, context):
        print 'serving', self.flavor, 'to', context.table

class CoffeeOrderContext(object):
    def __init__(self, table):
        self.table = table

class CoffeeFlavorFactory(object):
    flavors = {}
    def get_flavor(self, name):
        flavor = self.flavors.get(name)
        if not flavor:
            flavor = CoffeeFlavor(name)
            self.flavors[name] = flavor
        return flavor

    def total_made(self):
        return len(self.flavors.keys())

class App(object):
    def __init__(self):
        self.flavors = {}
        self.tables = {}
        self.orders = 0
        self.factory = CoffeeFlavorFactory()

    def take_orders(self, flavor_in, table):
        self.flavors[self.orders] = self.factory.get_flavor(flavor_in)
        self.tables[self.orders] = CoffeeOrderContext(table)
        self.orders +=  1

    def main(self):
        self.take_orders("capuccino", 2)
        self.take_orders("frappe", 10)
        self.take_orders("xpresso", 3)
        
        for i in range(self.orders):
            self.flavors[i].serve_coffee(self.tables[i])

        print
        print 'total:', self.factory.total_made()

if __name__ == '__main__':
    app = App()
    app.main()
    



