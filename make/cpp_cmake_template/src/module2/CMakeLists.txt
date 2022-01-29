add_library (module2
   STATIC
   mod2c1.cpp
   mod2c2.cpp
)

set_target_properties(module2 PROPERTIES ARCHIVE_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/obj")
