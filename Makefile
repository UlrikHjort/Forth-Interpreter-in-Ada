# Makefile for Forth Interpreter in Ada
# Copyright (C) 2026 By Ulrik Hørlyk Hjort

GNATMAKE = gnatmake
GNATFLAGS = -g -gnat2012
GCC = gcc
CFLAGS = -c -fPIC
SRC_DIR = src
BIN_DIR = bin
TARGET = $(BIN_DIR)/forth
MAIN = main.adb
TERMINAL_C = $(SRC_DIR)/terminal.c
TERMINAL_O = $(BIN_DIR)/terminal.o

.PHONY: all clean run test

all: $(TARGET)

$(TERMINAL_O): $(TERMINAL_C)
	@mkdir -p $(BIN_DIR)
	$(GCC) $(CFLAGS) $(TERMINAL_C) -o $(TERMINAL_O)

$(TARGET): $(SRC_DIR)/*.adb $(SRC_DIR)/*.ads $(TERMINAL_O)
	@mkdir -p $(BIN_DIR)
	cd $(SRC_DIR) && $(GNATMAKE) $(GNATFLAGS) $(MAIN) -D ../$(BIN_DIR) -o ../$(TARGET) -largs ../$(TERMINAL_O)

clean:
	rm -rf $(BIN_DIR)
	rm -f $(SRC_DIR)/*.ali $(SRC_DIR)/*.o $(SRC_DIR)/b~*

run: $(TARGET)
	$(TARGET)

test: $(TARGET)
	./run_tests.sh

.DEFAULT_GOAL := all
