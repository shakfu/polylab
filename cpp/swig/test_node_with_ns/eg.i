
%module eg

%{

#include "eg.h"

%}

%include "eg.h"
%include "std_pair.i"

namespace dialect {

extern dimensions Node::getDimensions(void);

}

%template() std::pair <double,double>;
%template(dimensions) std::pair <double,double>;
