cmake_minimum_required(VERSION 3.18...3.22)
project(my_project) # Replace 'my_project' with the name of your project

set(CMAKE_CXX_STANDARD 17)

find_package(Python 3.8 COMPONENTS Interpreter Development.Module REQUIRED)

if (NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGURATION_TYPES)
    set(CMAKE_BUILD_TYPE Release CACHE STRING "Choose the type of build." FORCE)
    set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS "Debug" "Release" "MinSizeRel" "RelWithDebInfo")
endif()

option(ENABLE_NANOBIND "enable noanobind")

if(ENABLE_NANOBIND)
    # Detect the installed nanobind package and import it into CMake
    execute_process(
        COMMAND "${Python_EXECUTABLE}" -m nanobind --cmake_dir
        OUTPUT_STRIP_TRAILING_WHITESPACE OUTPUT_VARIABLE NB_DIR)
    list(APPEND CMAKE_PREFIX_PATH "${NB_DIR}")
    find_package(nanobind CONFIG REQUIRED)

    nanobind_add_module(my_ext my_ext.cpp)
    target_compile_definitions(my_ext
        PUBLIC
        ENABLE_NANOBIND
    )

else()
    execute_process(
        COMMAND "${Python_EXECUTABLE}" -m pybind11 --cmakedir
        OUTPUT_STRIP_TRAILING_WHITESPACE OUTPUT_VARIABLE pybind11_DIR)
    list(APPEND CMAKE_PREFIX_PATH "${pybind11_DIR}")
    find_package(pybind11 CONFIG)
    pybind11_add_module(my_ext my_ext.cpp)

endif()

target_include_directories(my_ext
    PUBLIC
    ${CMAKE_SOURCE_DIR}/choc/audio
)