CC = clang
CXX = clang++

debug:   CFLAGS = -g -O0 -DDEBUG -DDIAGNOSTICS -DSLOW
release: CFLAGS = -O3 -march=native -DRELEASE -DFAST

LLVM_VERSION := 6.0

ifeq ($(shell which llvm-config),)
	LLVM_CONFIG := llvm-config-$(LLVM_VERSION)
else
	LLVM_CONFIG := llvm-config
endif

LLVM_CXXFLAGS = $(shell $(LLVM_CONFIG) --cxxflags)
LLVM_CXXLFLAGS = $(shell $(LLVM_CONFIG) --ldflags --link-static --system-libs --libs)

LFLAGS =
DISABLED_WARNINGS = -Wno-writable-strings -Wno-switch

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
	$(CC) src/main.c -c -o core.o $(CFLAGS) -DKAI_BINARY $(LFLAGS) $(DISABLED_WARNINGS)
llvm.o:
	$(CXX) src/llvm.cpp -c -o llvm.o $(LLVM_CXXFLAGS) $(DISABLED_WARNINGS)

tests: clean
	@./tools/gen_test_main.sh > $(TEST_MAIN)
	@$(CC) $(TEST_MAIN) -c -o core.o $(CFLAGS) -DTEST $(LFLAGS) $(DISABLED_WARNINGS)
	@$(CXX) src/llvm.cpp -c -o llvm.o $(LLVM_CXXFLAGS) $(DISABLED_WARNINGS)
	@$(CXX) -o $(TEST_TARGET) core.o llvm.o $(LLVM_CXXFLAGS) $(LLVM_CXXLFLAGS)
	@./$(TEST_TARGET) 2> $(TEST_LOG)
	@rm -f $(TEST_TARGET) $(TEST_MAIN)

clean:
	rm -f $(TARGET) core.o llvm.o $(TEST_TARGET) $(TEST_LOG) $(TEST_MAIN)
	
.PHONY: all clean debug release tests
