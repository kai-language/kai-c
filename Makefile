CC = clang

debug:   CFLAGS = -g -O0 -DDEBUG -DDIAGNOSTICS -DSLOW
release: CFLAGS = -O3 -march=native -DRELEASE -DFAST

LFLAGS =
DISABLED_WARNINGS = -Wno-writable-strings -Wno-switch

TARGET = kai

all: debug

debug:   clean $(TARGET)
release: clean $(TARGET)

$(TARGET):
	$(CC) src/main.cpp -o $(TARGET) $(CFLAGS) -DKAI_BINARY $(LFLAGS) $(DISABLED_WARNINGS)

clean:
	rm -f $(TARGET)
	
.PHONY: all clean debug release
