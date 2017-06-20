
set Investments;

param NumNodes := 21;
param NumScen := 64;

param b;
param G;
param q;
param r;

param NumOutcome := 4;
param Return{0 .. NumOutcome-1, Investments}; # := exp(Normal01())/10;

var x{1 .. NumNodes,Investments} >= 0;
var y{1 .. NumScen} >= 0;
var w{1 .. NumScen} >= 0;

param A{k in 2 .. NumNodes} = ceil((k-1)/NumOutcome);
param A2{s in 1 .. NumScen} = 5 + ceil(s/NumOutcome);
param O{k in 2 .. NumNodes} = (k-2) mod NumOutcome;
param O2{s in 1 .. NumScen} = (s-1) mod NumOutcome;

maximize ExpectedWealth:
	 sum{s in 1 .. NumScen} 1/NumScen * (q * y[s] - r * w[s]);

subject to InvestAllMoney:
	sum{i in Investments} x[1,i] = b;

subject to WealthBalance1{k in 2 .. NumNodes}:
	sum{i in Investments} x[k,i] = sum{i in Investments} Return[O[k],i] * x[A[k],i];

subject to Shortage{s in 1 .. NumScen}:
	sum{i in Investments} Return[O2[s],i] * x[A2[s],i] - y[s] + w[s] = G;
	



