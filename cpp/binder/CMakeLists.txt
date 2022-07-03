cmake_minimum_required(VERSION 3.4...3.18)
project(demo LANGUAGES CXX)

find_package(pybind11 REQUIRED)

pybind11_add_module(demo
    src/models.cpp
    bind/models.cpp
    bind/demo.cpp
)

include_directories(
    include
)


