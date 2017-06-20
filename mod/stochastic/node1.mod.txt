var x >= 0;
var w >= 0;
var y >= 0;
var theta >= 0;

param yin := 1;

minimize TheObj:
	 x + 3 * w + 0.5 * y + theta;

subject to UB:
	x <= 2;

subject to Inven:
	yin + x + w - y = 1;

#subject to Cut1:
#	theta >= 3 - 2 * y;

#subject to Cut2:
#	theta >= 3/2 - 1/2 * y;





