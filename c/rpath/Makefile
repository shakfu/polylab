.PHONY: all clean test test_runtime

all: test


dylib:
	@echo "the install_name section here is key"
	@clang -dynamiclib -std=gnu99 ratings.c \
		-current_version 1.0 \
		-compatibility_version 1.0 \
		-fvisibility=hidden \
		-install_name @rpath/libRatings.A.dylib \
		-o libRatings.A.dylib

dependent:
	@echo "location of dylib is specified as in the libs directory."
	@clang dependent.c -L. -lRatings.A -Wl,-rpath,@executable_path/libs -o dependent

runtime:
	@clang -o runtime runtime.c


test: dylib dependent
	@mkdir libs && mv libRatings.A.dylib ./libs
	@./dependent


test_runtime: dylib runtime
	@echo "needs to be next to dylib because it hardcoded at the c level."
	@./runtime

clean:
	@rm -rf libs dependent runtime libRatings.A.dylib

