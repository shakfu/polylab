# sets
set MENU;
set MENUITEM;
set INGREDIENT;
set EQUIPMENT;
set LABOR;
set CUSTOMER;

# parameters
param menu_cost_serving {MENU} >= 0;
param menu_n {MENU}, integer, >= 0;

param menuitem_cost {MENUITEM} >= 0;
param menuitem_demand {MENUITEM, MENU}, integer, >= 0;
param menuitem_total_demand {MENUITEM} >= 0;
param menuitem_total_cost {MENUITEM} >= 0;
param menuitem_margin {MENUITEM} >= 0;

param ingredient_cost_unit {INGREDIENT} >= 0;
param menuitem_ingredient_units {MENUITEM, INGREDIENT} >= 0;
param menuitem_ingredient_cost {MENUITEM} >= 0;

param equipment_cost_hr {EQUIPMENT} >= 0;
param equipment_avail_hrs {EQUIPMENT} >= 0;
param menuitem_equipment_hrs {MENUITEM, EQUIPMENT} >= 0;
param menuitem_equipment_cost {MENUITEM} >= 0;

param labor_cost_hr {LABOR} >= 0;
param labor_avail_hrs {LABOR} >= 0;
param menuitem_labor_hrs {MENUITEM, LABOR} >= 0;
param menuitem_labor_cost {MENUITEM} >= 0;

# decision variable
var menu_menuitems {MENUITEM, MENU}, binary;

# objective
#maximize Total_Profit:
#    sum {j in MENUITEM} menuitem_margin[j];

minimize Total_Cost:
    sum {j in MENUITEM} menuitem_total_cost[j];

# constraints
subject to menuitems_per_menu {i in MENU}:
    sum {j in MENUITEM} menu_menuitems[j, i] >= 3;


subject to equipment_capacity {i in EQUIPMENT}:
    sum {j in MENUITEM} menuitem_equipment_hrs[j, i] <= equipment_avail_hrs[i];
    

subject to labor_capacity {i in LABOR}:
    sum {j in MENUITEM} menuitem_labor_hrs[j, i] <= labor_avail_hrs[i];


display menu_menuitems > Sheet;
