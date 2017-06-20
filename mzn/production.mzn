function var int: sqr(var int:x) = x * x;

% number of different products
int: nproducts;
set of int: PRODUCT = 1..nproducts;

% profit per unit for each product
array[PRODUCT] of int: profit;
array[PRODUCT] of string: product_name;

% number of resources
int: nresources;
set of int: RESOURCE = 1..nresources;

% amount of each resource availble
array[RESOURCE] of int: capacity;
array[RESOURCE] of string: resource_name;

% units of each resource required to produce 1 unit of product
array[PRODUCT, RESOURCE] of int: consumption;
constraint assert(
  forall (r in RESOURCE, p in PRODUCT)(consumption[p,r] >= 0)
  , "Error: negative consumption");

% bound on number of Products
int: mproducts = max (p in PRODUCT)
                     (min (r in RESOURCE where consumption[p,r] > 0) 
                          (capacity[r] div consumption[p,r]));

% Variables: how much should we make of each product
array[PRODUCT] of var 0..mproducts: produce; 
array[RESOURCE] of var 0..max(capacity): used;

% Production cannot use more than the available Resources: 
constraint forall (r in RESOURCE) (
    used[r] = sum (p in PRODUCT)(consumption[p, r] * produce[p]));

constraint forall (r in RESOURCE) (used[r] <= capacity[r]);

% Maximize profit
solve maximize sum (p in PRODUCT) (profit[p]*produce[p]);

% output
output [show(product_name[p]) ++ " = " ++ show(produce[p]) ++ ";\n" | p in PRODUCT]     
       ++
       [show(resource_name[r]) ++ " = " ++ show(used[r]) ++ ";\n" | r in RESOURCE];

