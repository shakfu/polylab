

.PHONY: build bind clean


build: bind
	@echo "building via cmake"
	@mkdir -p build && 	\
		cd build && 	\
		cmake .. && 	\
		make


bind: clean
	@mkdir -p bind
	@binder 					\
	    --root-module demo 		\
	    --prefix $(PWD)/bind 	\
	    --bind models 			\
	    all_includes.hpp 		\
	    -- -std=c++14 -I$(PWD)/include -DNDEBUG

clean:
	@rm -rf build bind