param nSCEN;
set SCEN;

param nCut >= 0 integer;
param scenIx integer;

var x1 >= 0, := 2;
var x2 >= 0, := 2;
var theta >= 0;  #(Since q >=0, y >= 0)

var y1{SCEN} >= 0;
var y2{SCEN} >= 0;


param q1;
param q2;
param p{SCEN} default 1/card(SCEN);
param omega1{SCEN};
param omega2{SCEN};

param tcoef0{SCEN} default 0;
param tcoef1{SCEN} default 0;
param tcoef2{SCEN} default 0;

param coef0{1..nCut} default 0;
param coef1{1..nCut} default 0;
param coef2{1..nCut} default 0;


minimize SubObj{s in SCEN}:
         q1 * y1[s] + q2 * y2[s];

subject to c1{s in SCEN}:
        omega1[s] * x1 + x2 + y1[s] >= 7;
	
subject to c2{s in SCEN}:
        omega2[s] * x1 + x2 + y2[s] >= 4;


minimize MasterObj:
	 x1 + x2 + theta;

subject to Cuts {k in 1..nCut}:
	theta >= coef0[k] - coef1[k] * x1 - coef2[k] * x2;

	

