# Project Title

Assembly RPG

## Description

A Role-Playing Game made in x86_64 Assembly, targeted for linux

### Installing/Dependencies

* Download [NASM](https://www.nasm.us/)
* Download the ASM file
* You may also need to install fbdev and/or add your user to the video group depending on permissions


#### Executing the Program
* Navigate to the directory the asm file was downloaded to
** Feel free to move it to another folder
* In your terminal, type:
```
nasm -f elf64 RPG.asm -o RPG.o
```
```
ld RPG.o -o RPG
```
```
./RPG
```

## Issues
* If you run into any issues, please do the following:

* Ensure gdb in installed
* Recompile the program with debug symbols:
```
nasm -f elf64 -g -F dwarf RPG.asm -o RPG.o
```
```
ld RPG.o -o RPG
```
```
gdb ./RPG
```

* In gdb, run:
```
bt
```
and
```
info registers
```

* Include the results in your Issue, as well as your computer's architecture