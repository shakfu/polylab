#include "choc_Oscillators.h"
#define SAMPLE_RATE 44100


#ifdef ENABLE_NANOBIND

#include <nanobind/nanobind.h>
namespace nb = nanobind;

#else

#include <pybind11/pybind11.h>
namespace py = pybind11;

#endif


int add(int a, int b) { return a + b; }

#ifdef ENABLE_NANOBIND

NB_MODULE(my_ext, m) {
    m.def("add", &add);
    nb::class_<choc::oscillator::Sine<float>>(m, "Sine")
        .def(nb::init<>())
        .def("setFrequency", &choc::oscillator::Sine<float>::setFrequency)
        .def("getSample", &choc::oscillator::Sine<float>::getSample);
}

#else

PYBIND11_MODULE(my_ext, m) {
    m.def("add", &add);
    py::class_<choc::oscillator::Sine<float>>(m, "Sine")
        .def(py::init<>())
        .def("setFrequency", &choc::oscillator::Sine<float>::setFrequency)
        .def("getSample", &choc::oscillator::Sine<float>::getSample);
}

#endif

// int main()
// {
//     choc::oscillator::Sine<float> sine;
//     choc::oscillator::Saw<float> saw;
//     choc::oscillator::Square<float> sqr;
//     choc::oscillator::Triangle<float> tri;

//     sine.setFrequency(220.f, SAMPLE_RATE);
//     saw.setFrequency(220.f, SAMPLE_RATE);
//     sqr.setFrequency(220.f, SAMPLE_RATE);
//     tri.setFrequency(220.f, SAMPLE_RATE);

//     for (int i = 0; i < 4410; ++i) {
//         printf("%d: %f %f %f %f\n", i, 
//             sine.getSample(), saw.getSample(), sqr.getSample(), tri.getSample());
//     }
// }