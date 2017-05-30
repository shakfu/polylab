# A TRANSPORTATION PROBLEM
#
# This problem finds capacity least cost shipping schedule that meets
# requirements at markets and supplies at factories.
#
#  References:
#              Dantzig G B, "Linear Programming and Extensions."
#              Princeton University Press, Princeton, New Jersey, 1963,
#              Chapter 3-3.

set PLANT;
/* plants */

set MARKET;
/* markets */

param capacity{i in PLANT};
/* capacity of plant i in tons */

param demand{j in MARKET};
/* demand at market j in tons */

param distance{i in PLANT, j in MARKET};
/* distance in kilometers */

param freight_cost;
/* freight in dollars per case per thousand miles */

param transport_cost{i in PLANT, j in MARKET} := freight_cost * distance[i,j] / 1000;
/* transport cost in thousands of dollars per ton */

var quantity{i in PLANT, j in MARKET} >= 0;
/* shipment quantities in tons */

minimize cost: sum{i in PLANT, j in MARKET} transport_cost[i,j] * quantity[i,j];
/* total transportation costs in thousands of dollars */

s.t. supply_limit{i in PLANT}: sum{j in MARKET} quantity[i,j] <= capacity[i];
/* observe supply limit at plant i */

s.t. demand_satisfied{j in MARKET}: sum{i in PLANT} quantity[i,j] >= demand[j];
/* satisfy demand at market j */

data;

set PLANT := Seattle San-Diego;

set MARKET := New-York Chicago Topeka;

param capacity := Seattle     350
                  San-Diego   600;

param demand := New-York    325
                Chicago     300
                Topeka      275;

param distance :              New-York   Chicago   Topeka :=
                  Seattle     2.5        1.7       1.8
                  San-Diego   2.5        1.8       1.4  ;

param freight_cost := 90;

end;
