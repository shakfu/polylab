#include <models.h>
#include <sstream> // __str__

#include <functional>
#include <pybind11/pybind11.h>
#include <string>

#ifndef BINDER_PYBIND11_TYPE_CASTER
	#define BINDER_PYBIND11_TYPE_CASTER
	PYBIND11_DECLARE_HOLDER_TYPE(T, std::shared_ptr<T>)
	PYBIND11_DECLARE_HOLDER_TYPE(T, T*)
	PYBIND11_MAKE_OPAQUE(std::shared_ptr<void>)
#endif

void bind_models(std::function< pybind11::module &(std::string const &namespace_) > &M)
{
	{ // demo::Person file:models.h line:6
		pybind11::class_<demo::Person, std::shared_ptr<demo::Person>> cl(M("demo"), "Person", "");
		cl.def( pybind11::init( [](){ return new demo::Person(); } ) );
		cl.def("say", (void (demo::Person::*)()) &demo::Person::say, "C++: demo::Person::say() --> void");
	}
	{ // demo::Rectangle file:models.h line:13
		pybind11::class_<demo::Rectangle, std::shared_ptr<demo::Rectangle>> cl(M("demo"), "Rectangle", "");
		cl.def( pybind11::init<double, double>(), pybind11::arg("w"), pybind11::arg("h") );

		cl.def("area", (double (demo::Rectangle::*)()) &demo::Rectangle::area, "C++: demo::Rectangle::area() --> double");
		cl.def("show", (void (demo::Rectangle::*)()) &demo::Rectangle::show, "C++: demo::Rectangle::show() --> void");
	}
}
