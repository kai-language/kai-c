CC = clang
CXX = clang++

target = kai

sources := $(wildcard src/*.c)
disasms := $(pathsubst src/%.c, src/%.S, $(sources))

objdir = .build
objects := $(patsubst src/%.c, $(objdir)/%.o, $(sources))

includes = -Iincludes/ -Ideps/uu.spdr/include

ignored = -Wno-writable-strings -Wno-switch -Wno-c11-extensions -Wno-c99-extensions
cflags = -g -O0 -std=c11 $(includes) -DDEBUG $(ignored) $(CFLAGS)
cxxflags = -std=c++11 -stdlib=libc++ $(ignored)

test_main = test_main.c
test_log = tests.log

release: cflags = -O3 -std=c11 $(includes) -march=native -DRELEASE $(CFLAGS)

all: mkdirs $(target)

release: clean all

# compile
$(objdir)/%.o: src/%.c
	$(CC) $(cflags) -o $@ -c $<

# link
$(target): $(objects)
	$(CC) $(cflags) -o $@ $^

# disassembly
src/%.S: src/%.c
	$(CC) $(cflags) -o $@ -S $<
	
disasm: $(disasms)

unity:
	$(CC) $(cflags) -o $(target) unity.c

mkdirs:
	@mkdir -p $(objdir)

mktests: tools/mktests
	./tools/mktests src > test_main.c

mkxctests: tools/mkxctests
	./tools/mkxctests src > test_xcmain.c

tests: clean tools
	@./tools/mktests $(shell find src -maxdepth 1 -type d) > $(test_main)
	@$(CC) -o $@ $(cflags) -DTEST $(test_main)
	@./$@ 2>&1 $(test_log)
	@rm $@ $(test_main)

tools: tools/mktests tools/mkxctests

tools/mktests:
	$(CXX) $(cxxflags) -o $@ tools/mktests.cpp

tools/mkxctests:
	$(CXX) $(cxxflags) -o $@ tools/mkxctests.cpp

generate_db: clean
	intercept-build --override-compiler make CC=intercept-cc CXX=intercept-c++ all

.PHONY: clean

clean:
	rm -rf $(objdir) $(test_log) $(test_main)
 
# Debugging
## allows you to print makefile variables with make-VARIABLE
print-% : ; $(info $* is a $(flavor $*) variable set to [$($*)]) @true

