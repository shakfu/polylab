# service optimization model
#
# Finds optimal solution for maximizing business unit's Profit


# Sets
set SERVICE;
set WORK;
set EQUIPMENT;
set MATERIAL;

# Input Parameters
param demand                {SERVICE};
param markup                {SERVICE};

param work_hrs_required     {WORK, SERVICE};
param workers               {WORK};
param work_cost_hr          {WORK};
param worker_hrs_year;      # per worker

param equipment_hrs_required  {EQUIPMENT, SERVICE};
param equipments              {EQUIPMENT};
param equipment_cost_hr       {EQUIPMENT};    
param equipment_hrs_year;     # per equipment

param materials_required    {MATERIAL, SERVICE};
param materials             {MATERIAL};
param material_cost         {MATERIAL};

# Calculated Parameters
param service_work_cost {i in SERVICE} :=
    sum{j in WORK}
        work_cost_hr[j] * work_hrs_required[j, i];

param service_equipment_cost {i in SERVICE} := 
    sum{j in EQUIPMENT} 
        equipment_cost_hr[j] * equipment_hrs_required[j, i];

param service_material_cost {i in SERVICE} := 
    sum{j in MATERIAL} 
        material_cost[j] * materials_required[j,i];
        
param cost {i in SERVICE} :=
      service_material_cost[i]
    + service_equipment_cost[i]
    + service_work_cost[i];
    
#param revenue {i in SERVICE} := markup[i] * cost[i];

#param profit {i in SERVICE} := revenue[i] - cost[i];

param profit {i in SERVICE} := markup[i] * cost[i];


# Decision variables
var supply {i in SERVICE} >= 0, integer;

# Objective function
maximize z: sum{i in SERVICE} profit[i] * supply[i];

# Constraints
subject to work_capacity {j in WORK}:
    sum{i in SERVICE} work_hrs_required[j,i] * supply[i]
        <= worker_hrs_year * workers[j];

subject to equipment_capacity {j in EQUIPMENT}:
    sum{i in SERVICE} equipment_hrs_required[j,i] * supply[i]
        <= equipment_hrs_year * equipments[j];

subject to market {i in SERVICE}: supply[i] <= demand[i];

solve;

printf "supply :=\n" >> "Sheet";
printf {i in SERVICE}: "%s %s\n", i, supply[i] >> "Sheet";
printf ";\n" >> "Sheet";

#display supply;
#display material_cost;
#display service_material_cost;
#display service_work_cost;
#display service_equipment_cost;
#display service_cost;

# -------------------------------------------------------------------

end;
