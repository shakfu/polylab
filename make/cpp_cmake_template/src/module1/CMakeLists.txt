add_library (module1
   STATIC
   mod1c1.cpp
   mod1c2.cpp
)

set_target_properties(module1 PROPERTIES ARCHIVE_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/obj")
