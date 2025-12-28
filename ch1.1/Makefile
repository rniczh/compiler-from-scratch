.PHONY: all clean

all: gen
	./gen

gen: main_hello.cpp obj_writer.cpp
	g++ main_hello.cpp obj_writer.cpp --std=c++20 -o gen

clean:
	rm -f gen hello.o
