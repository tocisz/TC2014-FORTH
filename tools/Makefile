# Variables
CC = gcc
CFLAGS = -Wall -Wextra -pedantic -std=c11
TARGET = slowcat
SRC = slowcat.c

# Default target
all: $(TARGET)

# Compilation rule
$(TARGET): $(SRC)
	$(CC) $(CFLAGS) -o $(TARGET) $(SRC)

# Clean rule
clean:
	rm -f $(TARGET)

# Phony targets
.PHONY: all clean
