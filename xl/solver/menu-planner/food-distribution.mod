# Designing an Integrated Production/Distribution and Inventory Planning Model of Fixed-life Perishable Products

set PRODUCT;
set AGE; # starts from 1
set LINE; # production line
set DAY;
set PLANT;
set VEHICLE;
set DISTRO; # distribution center

# parameters
param last_period;
param capacity_hrs {LINE, DAY};
param capacity_consumption_hrs {PRODUCT, LINE};
param production_cost_unit {PRODUCT, LINE, PLANT};
param seq_setup_cost_hrs {PRODUCT, PRODUCT, LINE, PLANT};
param time_setup_cost_hrs {PRODUCT, PRODUCT, LINE, PLANT};
param product_spoilage_cost_unit {PRODUCT};
param product_lifetime {PRODUCT};
param product_shortage_cost_unit {PRODUCT};
param transport_cost {PLANT, VEHICLE, DISTRO};
param product_holding_cost {PRODUCT, DISTRO};
param product_occupied_space_unit {PRODUCT};
param distro_avail_space {DISTRO};
param vehicle_avail_space {VEHICLE};
param distro_product_demand {PRODUCT, DAY, DISTRO};

# decision variables
var quantity_produced {PRODUCT, LINE, PLANT, DAY};
var product_produced {PRODUCT, LINE, PLANT, DAY} binary;
var quantity_spoiled {PRODUCT, DAY, DISTRO};
var product_changed {PRODUCT, PRODUCT, LINE, PLANT, DAY} binary;
var quantity_shipped {PRODUCT, VEHICLE, PLANT, DISTRO, DAY};
var inventory_aged {PRODUCT, AGE, DAY, DISTRO};
var inventory {PRODUCT, DAY, DISTRO};
var product_shortage {PRODUCT, DAY, DISTRO};

# objective function (1)
minimize Z:
    sum {k in PRODUCT, kk in PRODUCT, l in LINE, p in PLANT, d in DAY}
    seq_setup_cost_hrs[k, kk, l, p] * product_changed[k, kk, l, p, d]
    +
    sum {k in PRODUCT, d in DAY, dc in DISTRO}
    product_holding_cost[k, dc] * inventory[k, d, dc]
    +
    sum {k in PRODUCT, d in DAY, dc in DISTRO}
    product_shortage_cost_unit[k] * product_shortage[k, d, dc]
    +
    sum {k in PRODUCT, d in DAY, dc in DISTRO}
    product_spoilage_cost_unit[k] * quantity_spoiled[k, d, dc]
    +
    sum {p in PLANT, v in VEHICLE, k in PRODUCT, d in DAY, dc in DISTRO}
    transport_cost[p, v, dc] * quantity_shipped[k, v, p, dc, d]
    +
    sum {k in PRODUCT, l in LINE, p in PLANT, d in DAY}
    production_cost_unit[k, l, p] * quantity_produced[k, l, p, d];

# constraints
# (2)
subject to Prod_Capacity {k in PRODUCT, l in LINE, p in PLANT, d in DAY}:
    quantity_produced[k, l, p, d] <=
        (capacity_hrs[l,d] / capacity_consumption_hrs[k, l])
        * product_produced[k, l, p, d];

# (3)
subject to Setup_Cost {l in LINE, p in PLANT, d in DAY}:
    sum {k in PRODUCT, kk in PRODUCT}
    time_setup_cost_hrs[k, kk, l, p] * product_changed[k, kk, l, p, d]
    +
    sum {k in PRODUCT, d in DAY} capacity_consumption_hrs[l,k] * quantity_produced[k,l,p,d]
    <= capacity_hrs[l,d];

# (4)
subject to Min_Production {l in LINE, p in PLANT, d in DAY}:
    sum {k in PRODUCT} product_produced[k, l, p, d] = 1;

# (5)
subject to Prod_Changes {k in PRODUCT, kk in PRODUCT, l in LINE, p in PLANT, d in DAY}
    product_changed[k, kk, l, p, d] >=
        product_produced[k, l, p, d] + product_produced[kk, l, p, prev(d)] - 1

# (6)
subject to Initial_Prod_State {k in PRODUCT, l in LINE, p in PLANT, d in DAY}
    product_produced[k, l, p, first(d)] = 0;

# (7)
subject to Prods_Shipped {k in PRODUCT, p in PLANT, d in DAY}:
    sum {l in LINE} quantity_produced[k, l, p, d] =
    sum {dc in DISTRO, v in VEHICLE} quantity_shipped[k, v, d, p, dc];

# (8)
subject to Distro_Capacity {d in DAY, dc in DISTRO}:
    sum {k in PRODUCT} product_occupied_space_unit[k] * inventory[k, d, dc]
        <= distro_avail_space[dc];

# (9)
subject to Vehicle_Capacity {v in VEHICLE, p in PLANT, dc in DISTRO, d in DAY}:
    sum {k in PRODUCT} product_occupied_space_unit[k] * quantity_shipped[k, v, d, p, dc]
        <= vehicle_avail_space[v];

# (10)
subject to Aged_Inventory_Levels {k in PRODUCT, dc in DISTRO, d in DAY}:
    sum {b in AGE: b <= product_lifetime[k]} inventory_aged[k, b, d, dc] = sum {b in AGE: b <= product_lifetime[k]-1} inventory_aged[k, b, prev(d), c]
                                                 + sum {v in VEHICLE, p in PLANT} quantity_produced[k, v, d, p, dc]
                                                 - distro_product_demand[k, d, dc];
# (11) FIFO Policy: demand fulfilled first
subject to Fifo_policy1 {k in PRODUCT, dc in DISTRO, d in DAY}:
    inventory[p, 1, d, dc] =
        sum {v in VEHICLE, p in PLANT} quantity_shipped[p, v, d, p, dc]
        - max(0, distro_product_demand[k, d, dc]
                 - sum {b in AGE: b <= product_lifetime[k]} inventory_aged[k, b, prev(d), c]);

# (12) FIFO Policy
subject to Fifo_policy2 {k in PRODUCT, dc in DISTRO, d in DAY, b in 2 .. product_lifetime[k]}:
    inventory_aged[k, b, d, dc] =
    inventory_aged[k, b-1, d-1, dc] - max(0, distro_product_demand[k, d, dc]
             - sum {b in AGE: b <= product_lifetime[k]} inventory_aged[k, b, prev(d), c]);

# (13) starting age of all inventories = 0
subject to Initial_Aged_Inventory {k in PRODUCT, d in DAY, dc in DISTRO}:
    inventory_aged[k, 0, d, dc] = 0;
