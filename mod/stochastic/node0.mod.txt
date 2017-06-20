var x >= 0, <= 2;
var w >= 0;
var y >= 0;
var theta >= 0;

minimize TheObj:
	 x + 3 * w + 0.5 * y + theta;

subject to Inven:
	x + w - y = 1;

#subject to Cut1:
#	theta >= 23/4 - 9/4 * y;

#subject to Cut2:
#	theta >= 23/4 - 2 * y;




