set DAY circular;
set BREAKFAST circular;
set LUNCH circular;
set DINNER circular;

param breakfast_cost {BREAKFAST};
param lunch_cost {LUNCH};
param dinner_cost {DINNER};

param mandays {DAY} integer >= 0;

param breakfast_cyclical_demand {BREAKFAST};
param lunch_cyclical_demand {LUNCH};
param dinner_cyclical_demand {DINNER};

param breakfast_schedule {i in BREAKFAST, t in DAY} binary default
    if ((t-(ord(i)-1)) mod card(BREAKFAST)) = 1 then 1 else 0;

param lunch_schedule {i in LUNCH, t in DAY} binary default
    if ((t-(ord(i)-1)) mod card(LUNCH)) = 1 then 1 else 0;

param dinner_schedule {i in DINNER, t in DAY} binary default
    if ((t-(ord(i)-1)) mod card(DINNER)) = 1 then 1 else 0;

var breakfast_supply {BREAKFAST, DAY} >= 0;

var lunch_supply {LUNCH, DAY} >= 0;

var dinner_supply {DINNER, DAY} >= 0;

var breakfast_daily_cost {t in DAY} =
    mandays[t] * sum {i in BREAKFAST} breakfast_cost[i] * breakfast_supply[i, t];

var lunch_daily_cost {t in DAY} =
    mandays[t] * sum {i in LUNCH} lunch_cost[i] * lunch_supply[i, t];

var dinner_daily_cost {t in DAY} =
    mandays[t] * sum {i in DINNER} dinner_cost[i] * dinner_supply[i, t];

var daily_cost {t in DAY} =
    breakfast_daily_cost[t] + lunch_daily_cost[t] + dinner_daily_cost[t];

minimize total_cost:
    sum {t in DAY} daily_cost[t];

subject to Breakfast_Daily_Demand {t in DAY}:
    sum {i in BREAKFAST} breakfast_supply[i, t] = mandays[t];

#subject to Breakfast_Cyclical_Demand {i in BREAKFAST}:
#    sum {j in DAY} breakfast_supply[i, j] >= breakfast_cyclical_demand[i];

subject to Breakfast_Rotation {i in BREAKFAST, t in DAY}:
    breakfast_supply[i, t] + breakfast_supply[i, next(t)] <= mandays[t];

subject to Breakfast_Schedule {i in BREAKFAST, t in DAY}:
    breakfast_schedule[i,t] * breakfast_supply[i,t] >= breakfast_supply[i,t];


subject to Lunch_Daily_Demand {t in DAY}:
    sum {i in LUNCH} lunch_supply[i, t] = mandays[t];

#subject to Lunch_Cyclical_Demand {i in LUNCH}:
#    sum {j in DAY} lunch_supply[i, j] >= lunch_cyclical_demand[i];

subject to Lunch_Rotation {i in LUNCH, t in DAY}:
    lunch_supply[i, t] + lunch_supply[i, next(t)] <= mandays[t];

subject to Lunch_Schedule {i in LUNCH, t in DAY}:
    lunch_schedule[i,t] * lunch_supply[i,t] >= lunch_supply[i,t];

subject to Dinner_Daily_Demand {t in DAY}:
    sum {i in DINNER} dinner_supply[i, t] = mandays[t];

#subject to Dinner_Cyclical_Demand {i in DINNER}:
#    sum {j in DAY} dinner_supply[i, j] >= dinner_cyclical_demand[i];

subject to Dinner_Rotation {i in DINNER, t in DAY}:
    dinner_supply[i, t] + dinner_supply[i, next(t)] <= mandays[t];

subject to Dinner_Schedule {i in DINNER, t in DAY}:
    dinner_schedule[i,t] * dinner_supply[i,t] >= dinner_supply[i,t];


solve;

display breakfast_supply > Sheet;
display lunch_supply > Sheet;
display dinner_supply > Sheet;

end;
