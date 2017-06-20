set DAY circular;
set MENU;
set BREAKFAST within MENU circular;
set LUNCH within MENU circular;
set DINNER within MENU circular;

param menu_cost {i in MENU};
param mandays {DAY} integer >= 0;
param daily_demand {DAY};
param menu_demand {MENU};

param rotation_schedule {i in MENU, t in DAY} binary;

param breakfast_schedule {i in 0 .. card(BREAKFAST)-1, t in DAY} binary default
    if ((t-i) mod card(BREAKFAST)) = 1 then 1 else 0;

param lunch_schedule {i in 0 .. card(LUNCH)-1, t in DAY} binary default
    if ((t-i) mod card(LUNCH)) = 1 then 1 else 0;

param dinner_schedule {i in 0 .. card(DINNER)-1, t in DAY} binary default
    if ((t-i) mod card(DINNER)) = 1 then 1 else 0;

var menu_supply {MENU, DAY} >= 0;

var daily_cost {t in DAY} =
    mandays[t] * sum {i in MENU} menu_cost[i] * menu_supply[i, t];


minimize total_cost:
    sum {t in DAY} daily_cost[t];

subject to Daily_Demand {t in DAY}:
    sum {i in MENU} menu_supply[i, t] = daily_demand[t];

subject to Breakfast_Requirement {t in DAY}:
    sum {i in BREAKFAST} menu_supply[i, t] = mandays[t];

subject to Lunch_Requirement {t in DAY}:
    sum {i in LUNCH} menu_supply[i, t] = mandays[t];

subject to Dinner_Requirement {t in DAY}:
    sum {i in DINNER} menu_supply[i, t] = mandays[t];

subject to Breakfast_Rotation {i in BREAKFAST, t in DAY}:
    menu_supply[i, t] + menu_supply[i, next(t)] <= mandays[t];

subject to Lunch_Rotation {i in LUNCH, t in DAY}:
    menu_supply[i, t] + menu_supply[i, next(t)] <= mandays[t];

subject to Dinner_Rotation {i in DINNER, t in DAY}:
    menu_supply[i, t] + menu_supply[i, next(t)] <= mandays[t];

#subject to Menu_Demand {i in MENU}:
#    sum {j in DAY} menu_supply[i, j] >= menu_demand[i];

subject to Schedule {i in MENU, t in DAY}:
    rotation_schedule[i,t] * menu_supply[i,t] >= menu_supply[i,t];

solve;

display menu_supply > Sheet;

end;
