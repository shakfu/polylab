'''
Supplier IKTVA formula


         A + B + C + D
IKTVA =  ------------- x 100
               E

    Where:

    A = Localized goods and services ($)
    B = Salaries paid to Locals ($)
    C = Training and Development of Locals ($)
    D = Supplier development spend ($)
    E = Revenue (spend from Client)

'''

def iktva(good_services, local_salaries, training, supplier_dev, client_revenue):
    return (good_services + local_salaries + training + supplier_dev) / client_revenue

tests = [
    # good_services, local_salaries, training, supplier_dev, client_revenue
    (100.0,          20.0,           10.0,     5.0,          200.0),
]

for args in tests:
    print iktva(*args)
