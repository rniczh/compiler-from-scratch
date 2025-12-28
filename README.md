# BCFS: Building Compiler From Scratch

An educational compiler built from scratch in C++.

This project adopts the bottom-up approach, starting from the ELF object file and ARM64 assembly, then working our way up to the source language.

## Blog Series

This project follows along with the blog series:

- [BCFS: Building Compiler From Scratch, The Beginning](https://rniczh.github.io/blog/20251206-building-compiler-from-scratch-the-beginning/index.html)
- [BCFS: Object File Generation and the ELF Format Part I](https://rniczh.github.io/blog/20251214-Object-File-Generation-and-the-ELF-Format-Part-I/index.html)

## [1] print hello world

```
cd ch1.1
```

### Build & Run

```bash
make
```

This will:
1. Compile to `gen` from `main_hello.cpp` and `obj_writer.cpp`
2. Run `./gen` to generate `hello.o`

### Link & Execute

```bash
gcc -o hello hello.o
./hello
```

Expected output:
```
hello world
```

### Clean

```bash
make clean
```
