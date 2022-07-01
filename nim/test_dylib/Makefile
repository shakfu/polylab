

all: test_dylib test_c

libcalc.dylib:
	@echo "build dylib from nim code: calc.nim -> libcalc.dylib"
	@nim c -d:release --opt:size --app:lib calc.nim


test_dylib: libcalc.dylib
	@echo "build c exec from dylib: test_calc.c -> libcalc.dylib -> test_dylib"
	@gcc -O3 -o test_dylib test_calc.c -lcalc -L.


test_c:
	@echo "build c exec from c: test_calc.c -> calc.c -> test_c"
	@gcc -O3 -o test_c test_calc.c calc.c


.PHONY: test_ctypes clean


test_ctypes:
	@echo "test using python ctypes: test_calc.py -> libcalc.dylib"
	@python3 test_calc.py


clean:
	@rm -f libcalc.dylib
	@rm -f test_c
	@rm -f test_dylib
