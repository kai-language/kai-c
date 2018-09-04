CC = clang
CXX = clang++
PREFIX=/usr/local

debug:   local_CFLAGS = -g -O0 -std=c11 -DDEBUG -DDIAGNOSTICS -DSLOW $(CFLAGS)
release: local_CFLAGS = -O3 -std=c11 -march=native -DRELEASE -DFAST $(CFLAGS)

LLVM_VERSION := 6.0

ifeq ($(shell which llvm-config),)
	LLVM_CONFIG := llvm-config-$(LLVM_VERSION)
else
	LLVM_CONFIG := llvm-config
endif

LLVM_CXXFLAGS = $(shell $(LLVM_CONFIG) --cxxflags)
LLVM_CXXLFLAGS = $(shell $(LLVM_CONFIG) --ldflags --link-static --system-libs --libs)

LFLAGS =
DISABLED_WARNINGS = -Wno-writable-strings -Wno-switch -Wno-c11-extensions -Wno-c99-extensions

TARGET = kai
TEST_TARGET = $(TARGET)_tests
TEST_MAIN = $(TARGET)_tests.c
TEST_LOG = $(TARGET)_tests.log

all: debug

debug:   clean $(TARGET)
release: clean $(TARGET)

$(TARGET): core.o llvm.o
	$(CXX) -o $(TARGET) core.o llvm.o $(LLVM_CXXFLAGS) $(LLVM_CXXLFLAGS)
core.o:
	$(CC) src/main.c -c -o core.o $(local_CFLAGS) -DKAI_BINARY $(LFLAGS) $(DISABLED_WARNINGS)
llvm.o:
	$(CXX) src/llvm.cpp -c -o llvm.o $(LLVM_CXXFLAGS) $(DISABLED_WARNINGS)

tools/genTests:
	$(CXX) -o tools/genTests tools/gen_test_main.cpp

tests: clean tools/genTests
	@./tools/genTests src/ > $(TEST_MAIN)
	@$(CC) $(TEST_MAIN) -c -o core.o  -DTEST $(LFLAGS) $(DISABLED_WARNINGS) $(local_CFLAGS)
	@$(CXX) src/llvm.cpp -c -o llvm.o $(LLVM_CXXFLAGS) $(DISABLED_WARNINGS)
	@$(CXX) -o $(TEST_TARGET) core.o llvm.o $(LLVM_CXXFLAGS) $(LLVM_CXXLFLAGS)
	@./$(TEST_TARGET) 2> $(TEST_LOG)
	@rm -f $(TEST_TARGET) $(TEST_MAIN)

install:
	cp $(TARGET) $(PREFIX)/bin/

clean:
	rm -f $(TARGET) core.o llvm.o $(TEST_TARGET) $(TEST_LOG) $(TEST_MAIN)
	
.PHONY: all clean debug release tests
