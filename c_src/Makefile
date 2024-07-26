# Makefile for x11_nif

# Compiler and flags
CC = gcc
CFLAGS = -Wall -fPIC
LDFLAGS = -shared -lX11
INCLUDE = -I /usr/lib/erlang/erts-*/include

# Target shared library
TARGET = x11_nif.so

# Source files
SRCS = x11_nif.c

# Object files
OBJS = $(SRCS:.c=.o)

# Default target
all: $(TARGET)

# Rule to create the shared library
$(TARGET): $(OBJS)
	$(CC) $(LDFLAGS) -o $@ $(OBJS)

# Rule to compile source files to object files
%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@ $(INCLUDE)

# Clean rule to remove build artifacts
clean:
	rm -f $(TARGET) $(OBJS)

.PHONY: all clean

