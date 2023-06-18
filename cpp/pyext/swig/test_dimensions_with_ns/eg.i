
%module eg

%{

#include "eg.h"

%}

%include "eg.h"
%include "std_pair.i"


%template() std::pair <double,double>;
%template(dimensions) std::pair <double,double>;
