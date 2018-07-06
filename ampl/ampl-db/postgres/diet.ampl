option solver cbc;
model diet.mod;

param ConnectionStr symbolic = "DSN=dietpg;";

table dietFoods "ODBC" (ConnectionStr) "foods":
   FOOD <- [food], cost IN, f_min IN, f_max IN,
   Buy OUT, Buy.rc ~ BuyRC OUT, {j in FOOD} Buy[j]/f_max[j] ~ BuyFrac;

table dietNutrs IN "ODBC" (ConnectionStr) "nutrients": NUTR <- [nutr], n_min, n_max;
table dietAmts IN "ODBC" (ConnectionStr) "amounts": [nutr, food], amt;

read table dietFoods;
read table dietNutrs;
read table dietAmts;
                  
solve;

write table dietFoods;
