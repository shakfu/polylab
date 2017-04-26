/* JSSP, Job-Shop Scheduling Problem */

/* Written in GNU MathProg by Andrew Makhorin <mao@gnu.org> */

/* The Job-Shop Scheduling Problem (JSSP) is to schedule a set of jobs
   on a set of machines, subject to the constraint that each machine can
   handle at most one job at a time and the fact that each job has a
   specified processing order through the machines. The objective is to
   schedule the jobs so as to minimize the maximum of their completion
   times.

   Reference:
   D. Applegate and W. Cook, "A Computational Study of the Job-Shop
   Scheduling Problem", ORSA J. On Comput., Vol. 3, No. 2, Spring 1991,
   pp. 149-156. */

param n, integer, > 0;
/* number of jobs */

param m, integer, > 0;
/* number of machines */

set JOB := 1..n;
/* set of jobs */

set MACHINE := 1..m;
/* set of machines */

param sigma{j in JOB, t in 1..m}, in MACHINE;
/* permutation of the machines, which represents the processing order
   of j through the machines: j must be processed first on sigma[j,1],
   then on sigma[j,2], etc. */

check{j in JOB, t1 in 1..m, t2 in 1..m: t1 <> t2}:
      sigma[j,t1] != sigma[j,t2];
/* sigma must be permutation */

param processing_time{j in JOB, a in MACHINE}, >= 0;
/* processing time of j on a */

var starting_time{j in JOB, a in MACHINE}, >= 0;
/* starting time of j on a */

s.t. ord{j in JOB, t in 2..m}:
      starting_time[j, sigma[j,t]] >= starting_time[j, sigma[j,t-1]] + processing_time[j, sigma[j,t-1]];
/* j can be processed on sigma[j,t] only after it has been completely
   processed on sigma[j,t-1] */

/* The disjunctive condition that each machine can handle at most one
   job at a time is the following:

      starting_time[i,a] >= starting_time[j,a] + processing_time[j,a]  or  starting_time[j,a] >= starting_time[i,a] + processing_time[i,a]

   for all i, j in JOB, a in M. This condition is modeled through binary
   variables SCHEDULE as shown below. */

var SCHEDULE{i in JOB, j in JOB, a in MACHINE}, binary;
/* SCHEDULE[i,j,a] is 1 if i scheduled before j on machine a, and 0 if j is
   scheduled before i */

param K := sum{j in JOB, a in MACHINE} processing_time[j,a];
/* some large constant */

display K;

s.t. phi{i in JOB, j in JOB, a in MACHINE: i <> j}:
      starting_time[i,a] >= starting_time[j,a] + processing_time[j,a] - K * SCHEDULE[i,j,a];
/* starting_time[i,a] >= starting_time[j,a] + processing_time[j,a] iff SCHEDULE[i,j,a] is 0 */

s.t. psi{i in JOB, j in JOB, a in MACHINE: i <> j}:
      starting_time[j,a] >= starting_time[i,a] + processing_time[i,a] - K * (1 - SCHEDULE[i,j,a]);
/* starting_time[j,a] >= starting_time[i,a] + processing_time[i,a] iff SCHEDULE[i,j,a] is 1 */

var makespan;
/* so-called makespan */

s.t. fin{j in JOB}: makespan >= starting_time[j, sigma[j,m]] + processing_time[j, sigma[j,m]];
/* which is the maximum of the completion times of all the jobs */

minimize obj: makespan;
/* the objective is to make makespan as small as possible */

data;

/* These data correspond to the instance ft06 (mt06) from:

   H. Fisher, G.L. Thompson (1963), Probabilistic learning combinations
   of local job-shop scheduling rules, J.F. Muth, G.L. Thompson (eds.),
   Industrial Scheduling, Prentice Hall, Englewood Cliffs, New Jersey,
   225-251 */

/* The optimal solution is 55 */

param n := 6;

param m := 6;

param sigma :  1  2  3  4  5  6 :=
          1    3  1  2  4  6  5
          2    2  3  5  6  1  4
          3    3  4  6  1  2  5
          4    2  1  3  4  5  6
          5    3  2  5  6  1  4
          6    2  4  6  1  5  3 ;

param processing_time     :  1  2  3  4  5  6 :=
          1    3  6  1  7  6  3
          2   10  8  5  4 10 10
          3    9  1  5  4  7  8
          4    5  5  5  3  8  9
          5    3  3  9  1  5  4
          6   10  3  1  3  4  9 ;


end;
