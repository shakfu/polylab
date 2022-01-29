add_subdirectory(module1)
add_subdirectory(module2)

add_executable(program program.cpp)

set_target_properties(program PROPERTIES RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/bin")


target_link_libraries(program
   PRIVATE
   module1
   module2
)
