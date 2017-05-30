
set JOB;
set MACHINE;

param n := card(JOB);
param m := card(MACHINE);

# permutation of the machines, which represents the processing order
# of j through the machines: j must be processed first on sigma[j,1],
# then on sigma[j,2], etc. */
param sigma{j in JOB, t in MACHINE};

display sigma;

# sigma must be permutation 
check{j in JOB, t1 in MACHINE, t2 in MACHINE: t1 <> t2}:
      sigma[j,t1] != sigma[j,t2];

param processing_time{j in JOB, a in MACHINE}, >= 0;

var starting_time{j in JOB, a in MACHINE}, >= 0;

s.t. ord{j in JOB, t in 2..m}:
    starting_time[j, sigma[j,t]] >= starting_time[j, sigma[j,t-1]] 
    + processing_time[j, sigma[j,t-1]];


var schedule{i in JOB, j in JOB, a in MACHINE}, binary;

param K := sum{j in JOB, a in MACHINE} processing_time[j,a];

display K;

s.t. phi{i in JOB, j in JOB, a in MACHINE: i <> j}:
      starting_time[i,a] >= starting_time[j,a] 
      + processing_time[j,a] 
      - K * schedule[i,j,a];


s.t. psi{i in JOB, j in JOB, a in MACHINE: i <> j}:
      starting_time[j,a] >= starting_time[i,a] 
      + processing_time[i,a] 
      - K * (1 - schedule[i,j,a]);


var makespan;

s.t. fin{j in JOB}: 
    makespan >= starting_time[j, sigma[j,m]] 
    + processing_time[j, sigma[j,m]];


minimize obj: makespan;

solve;

display makespan;

display starting_time;

#display schedule;

data;

# The optimal solution is 55 

set JOB := 1 2 3 4 5 6;

set MACHINE := 1 2 3 4 5 6;


param sigma :  1  2  3  4  5  6 :=
          1    3  1  2  4  6  5
          2    2  3  5  6  1  4
          3    3  4  6  1  2  5
          4    2  1  3  4  5  6
          5    3  2  5  6  1  4
          6    2  4  6  1  5  3 ;

param processing_time     
            :  1  2  3  4  5  6 :=
          1    3  6  1  7  6  3
          2   10  8  5  4 10 10
          3    9  1  5  4  7  8
          4    5  5  5  3  8  9
          5    3  3  9  1  5  4
          6   10  3  1  3  4  9 ;




end;

