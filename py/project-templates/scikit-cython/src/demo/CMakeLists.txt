
add_custom_command(
    OUTPUT _core.c
    COMMAND Python::Interpreter -m cython
          "${CMAKE_CURRENT_SOURCE_DIR}/_core.pyx" --output-file _core.c
    DEPENDS _core.pyx
)

python_add_library(_core MODULE _core.c WITH_SOABI)

install(TARGETS _core DESTINATION .)
