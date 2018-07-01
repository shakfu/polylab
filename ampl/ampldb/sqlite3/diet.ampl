option solver cbc;
model diet.mod;

param ConnectionStr symbolic = "DSN=dietsql3;";

table dietFoods "ODBC" (ConnectionStr) "Foods":
   FOOD <- [FOOD], cost IN, f_min IN, f_max IN,
   Buy OUT, Buy.rc ~ BuyRC OUT, {j in FOOD} Buy[j]/f_max[j] ~ BuyFrac;

table dietNutrs IN "ODBC" (ConnectionStr) "Nutrients": NUTR <- [NUTR], n_min, n_max;
table dietAmts IN "ODBC" (ConnectionStr) "Amounts": [NUTR, FOOD], amt;

read table dietFoods;
read table dietNutrs;
read table dietAmts;
                  
solve;

write table dietFoods;
