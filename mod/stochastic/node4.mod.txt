var x >= 0;
var w >= 0;
var y >= 0;

param yin := 0;

minimize TheObj:
	 x + 3 * w;

subject to UB:
	x <= 2;

subject to Inven:
	yin + x + w - y = 3;




