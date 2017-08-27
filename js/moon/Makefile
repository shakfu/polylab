JS=js
CLOSURE=@java -jar ~/bin/closure.jar
CLOSURE_FLAGS=--compilation_level SIMPLE_OPTIMIZATIONS \
			  --warning_level QUIET 				   							 \
			  --language_in ECMASCRIPT6 			               \
			  --jscomp_off globalThis

$(JS)/app.min.js: $(JS)/app.js
	@echo "Compressing app.js with closure"
	$(CLOSURE) $(CLOSURE_FLAGS) --js $(JS)/app.js --js_output_file $(JS)/app.min.js

.PHONY: clean

clean:
	@echo "cleaning up"
	@rm $(JS)/app.min.js
