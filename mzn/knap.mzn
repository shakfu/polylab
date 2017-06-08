%% test.mzn %%
int: n;                     % number of objects
set of int: OBJ = 1..n;
int: capacity;              % the capacity of the knapsack
array[OBJ] of int: profit;  % the profit of each object
array[OBJ] of int: size;    % the size of each object

var set of OBJ: x;
constraint sum(i in x)(size[i]) <= capacity;
var int: obj = sum(i in x)(profit[i]);
solve maximize obj;
