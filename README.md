
## edsktool is a tool designed for Extended DSK management

compilation: edsktool is a short onefile source written in very old ANSI-C style, do you really need a makefile?

this tool can map, explore, fix, modify, merge EDSK

## new native writer
- better support of unusual formats (can write SuperCycle floppy)
- almost all natives erased sectors formats supported
- can write track with many sectors sharing the same ID
- can mimic almost any kind of modified GAP
- standard sectors are verified (10 retries then stop)
- hexagon sectors are verified (no retry on this but not stop either)
- new pack loader in pure assembly
- 2 sides support
- multiple drives support

## write EDSK on a real CPC (two side with 3.5" or one side with internal 3")

./edsktool.exe myfloppy.edsk -export

This will generate an export\_definition.asm file to assemble with rasm

Drag'n'drop gendisk.bin and all pack\*.dat files on your M4 folder and RUN"GENDISK.BAS

