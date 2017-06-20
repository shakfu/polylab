set INVESTMENT;

param profit {INVESTMENT} >= 0;
param capital {INVESTMENT} >= 0;
param returns {i in INVESTMENT} = profit[i] / capital[i];
param budget;

param cond_xor1 {INVESTMENT} binary;
param cond_xor2 {INVESTMENT} binary;
param cond_ifxor {INVESTMENT} integer;
param cond_and {INVESTMENT} binary;

var yes_no {INVESTMENT} binary;

maximize Total_Profit:
    sum {i in INVESTMENT} profit[i] * yes_no[i];

subject to Budget_Limits:
    sum {i in INVESTMENT} capital[i] * yes_no[i] <= budget;

subject to OR_Condition_1:
    sum {i in INVESTMENT} cond_xor1[i] * yes_no[i] <= 1;

subject to OR_Condition_2:
    sum {i in INVESTMENT} cond_xor2[i] * yes_no[i] <= 1;

subject to IfXOR_Condition:
    sum {i in INVESTMENT} cond_ifxor[i] * yes_no[i] <= 0;

subject to AND_Condition:
    sum {i in INVESTMENT} cond_and[i] * yes_no[i] - 1 <= 1;
subject to AND_Condition2 {i in INVESTMENT: cond_and[i] >= 1}:
    cond_and[i] * yes_no[i] >= 1;

# WORKS with option solver ilogcp;
#subject to AND_Condition:
#    sum {i in INVESTMENT} cond_and[i] * yes_no[i] - 1 <= 1
#    and
#    forall {i in INVESTMENT: cond_and[i] >= 1} cond_and[i] * yes_no[i] >= 1;

# forced required
#subject to AND_Condition:
#    forall {i in INVESTMENT: cond_and[i] >= 1} cond_and[i] * yes_no[i] >= 1;

#option solver ilogcp;
