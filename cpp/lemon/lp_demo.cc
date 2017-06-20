/* -*- mode: C++; indent-tabs-mode: nil; -*-
*
* This file is a part of LEMON, a generic C++ optimization library.
*
* Copyright (C) 2003-2010
* Egervary Jeno Kombinatorikus Optimalizalasi Kutatocsoport
* (Egervary Research Group on Combinatorial Optimization, EGRES).
*
* Permission to use, modify and distribute this software is granted
* provided that this copyright notice appears in all copies. For
* precise terms see the accompanying LICENSE file.
*
* This software is provided "AS IS" with no warranty of any kind,
* express or implied, and with no claim as to its suitability for any
* purpose.
*
*/
#include <iostream>
#include <lemon/lp.h>
using namespace lemon;
int main()
{
// Create an instance of the default LP solver class
// (it will represent an "empty" problem at first)
	Lp lp;
// Add two columns (variables) to the problem
	Lp::Col x1 = lp.addCol();
	Lp::Col x2 = lp.addCol();
// Add rows (constraints) to the problem
	lp.addRow(x1 - 5 <= x2);
	lp.addRow(0 <= 2 * x1 + x2 <= 25);
// Set lower and upper bounds for the columns (variables)
	lp.colLowerBound(x1, 0);
	lp.colUpperBound(x2, 10);
// Specify the objective function
	lp.max();
	lp.obj(5 * x1 + 3 * x2);
// Solve the problem using the underlying LP solver
	lp.solve();
// Print the results
	if (lp.primalType() == Lp::OPTIMAL) {
		std::cout << "Objective function value: " << lp.
		    primal() << std::endl;
		std::cout << "x1 = " << lp.primal(x1) << std::endl;
		std::cout << "x2 = " << lp.primal(x2) << std::endl;
	} else {
		std::cout << "Optimal solution not found." << std::endl;
	}
	return 0;
}
