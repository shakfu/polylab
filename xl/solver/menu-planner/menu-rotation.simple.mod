set DAY; #circular
set MENU;
set BREAKFAST within MENU;
set LUNCH within MENU;
set DINNER within MENU;

# input params
param menu_cost {MENU} >  0; 
param mandays {DAY} integer >= 0;

# decision variable
var menu_supply {MENU, DAY} integer >= 0;

# calculated parameters
param daily_demand {i in DAY} := 3 * mandays[i];


minimize total_cost:
    sum {i in DAY} (mandays[i] * sum {j in MENU} menu_cost[j] * menu_supply[j,i]);

subject to Daily_Demand {i in DAY}:
    sum {j in MENU} menu_supply[j, i] = daily_demand[i];
    
subject to Breakfast_Requirement {i in DAY}:
    sum {j in BREAKFAST} menu_supply[j, i] = 1/3 * daily_demand[i]; 

subject to Lunch_Requirement {i in DAY}:
    sum {j in LUNCH} menu_supply[j, i] = 1/3 * daily_demand[i];

subject to Dinner_Requirement {i in DAY}:
    sum {j in DINNER} menu_supply[j, i] = 1/3 * daily_demand[i]; 

#subject to Daily_Rotation {i in DAY}:
#    menu_supply[1, i] + menu_supply[2, next(i)] <= 1/3 * daily_demand[i];


solve;

#display menu_supply;

data;

set DAY := 1 2 3 4 5 6 7;
set MENU := 1 2 3 4 5 6;
set BREAKFAST := 1 2;
set LUNCH := 3 4;
set DINNER := 5 6;

param menu_cost := 1 1.0,
                   2 2.0,
                   3 3.0,
                   4 4.0,
                   5 5.0, 
                   6 6.0;

param mandays := 1 100,
                 2 100,
                 3 100,
                 4 100,
                 5 100,
                 6 100,
                 7 100; 

end;
