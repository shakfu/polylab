# sets
set DAY;
set MENU;
set BREAKFAST within MENU;
set LUNCH within MENU;
set DINNER within MENU;

# params
param breakfast_cost {BREAKFAST} >= 0;
param lunch_cost {LUNCH} >= 0;
param dinner_cost {DINNER} >= 0;
param mandays {DAY} integer >= 0;
param manday_rate {DAY};
param day_cost {DAY};

# vars
var breakfast_m {DAY} integer;
var lunch_m {DAY} integer;
var dinner_m {DAY} integer;

# objective
minimize total_cost:
    sum {i in DAY} day_cost[i];

# constraints
subject to breakfast_limits {i in DAY}:
   1 <= breakfast_m[i] <= 5;

subject to lunch_limits {i in DAY}:
   1 <= lunch_m[i] <= 5;

subject to dinner_limits {i in DAY}:
   1 <= dinner_m[i] <= 5;
