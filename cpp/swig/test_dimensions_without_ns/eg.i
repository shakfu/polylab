
%module eg

%{

#include "eg.h"
	
%}

%include "eg.h"
%include "std_pair.i"

extern dimensions getDimensions(double x, double y);

%template() std::pair <double,double>;
%template(dimensions) std::pair <double,double>;