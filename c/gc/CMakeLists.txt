cmake_minimum_required(VERSION 3.19)

project(gc_example)

set(LOCAL_PREFIX ${CMAKE_SOURCE_DIR}/build/install/bdwgc)
set(INCLUDES ${LOCAL_PREFIX}/include)
set(LIBS ${LOCAL_PREFIX}/lib)

add_executable(demo
	src/demo.c
)

target_include_directories(demo
	PUBLIC
	${INCLUDES}
)

target_link_directories(demo
	PUBLIC
	${LIBS}
)

target_link_libraries(demo
	PUBLIC
	${LIBS}/libgc.a
	# ${LIBS}/libgctba.a
	# ${LIBS}/libgccpp.a  
)
