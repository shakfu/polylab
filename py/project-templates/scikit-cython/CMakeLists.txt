cmake_minimum_required(VERSION 3.5)
project(${SKBUILD_PROJECT_NAME} LANGUAGES C)

find_package(
  Python
  COMPONENTS Interpreter Development.Module
  REQUIRED
)

add_subdirectory(src)