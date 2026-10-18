cmake_minimum_required(VERSION 4.2)

project(forth_project)

add_executable(forth forth.c)

target_compile_options(forth PRIVATE -Wall -Wextra)

install(TARGETS forth RUNTIME DESTINATION bin)
