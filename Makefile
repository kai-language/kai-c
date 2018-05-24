CC = clang
CXX = clang++

debug:   CFLAGS = -g -O0 -DDEBUG -DDIAGNOSTICS -DSLOW
release: CFLAGS = -O3 -march=native -DRELEASE -DFAST

LLVM_CXXFLAGS = $(shell llvm-config --cxxflags)
LLVM_CXXLFLAGS = $(shell llvm-config --ldflags --link-static --system-libs --libs)

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

tests:
	@rm -f $(TEST_TARGET) $(TEST_LOG) $(TEST_MAIN)
	@./scripts/gen_test_main.sh > $(TEST_MAIN)
	@$(CC) $(TEST_MAIN) -o $(TEST_TARGET) $(CFLAGS) -DTEST $(LFLAGS) $(DISABLED_WARNINGS)
	@./$(TEST_TARGET) 2> $(TEST_LOG)
	@rm -f $(TEST_TARGET) $(TEST_MAIN)

clean:
	rm -f $(TARGET) core.o llvm.o
	
.PHONY: all clean debug release tests
