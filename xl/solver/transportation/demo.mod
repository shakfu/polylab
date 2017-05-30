set ORIG;   # origins
set DEST;   # destinations
set LINKS within {ORIG,DEST};


param supply {ORIG} >= 0;   # amounts available at origins
param demand {DEST} >= 0;   # amounts required at destinations
check: sum {i in ORIG} supply[i] = sum {j in DEST} demand[j];
param cost {LINKS} >= 0;   # shipment costs per unit
var Trans {LINKS} >= 0;    # units to be shipped

minimize Total_Cost:
  sum {(i,j) in LINKS} cost[i,j] * Trans[i,j];

subject to Supply {i in ORIG}:
  sum {(i,j) in LINKS} Trans[i,j] = supply[i];

subject to Demand {j in DEST}:
  sum {(i,j) in LINKS} Trans[i,j] = demand[j];

data;

param: ORIG: supply :=
    GARY  1400    CLEV  2600    PITT  2900 ;

param: DEST:  demand :=
    FRA   900   DET  1200   LAN   600    WIN  400
    STL  1700   FRE  1100   LAF  1000 ;

param: LINKS:  cost :=
    GARY DET 14
    CLEV FRA 27
    CLEV STL 26
    PITT FRA 24
    GARY LAN 11
    CLEV DET  9
    CLEV LAF 17
    PITT WIN 13
    GARY STL 16
    CLEV LAN 12
    GARY LAF  8
    CLEV WIN  9
    PITT STL 28;

