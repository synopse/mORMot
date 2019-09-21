@echo off

attrib -r sqlite3.o 
del sqlite3.o

rem set bcc=\dev\bcc\XE7
set bcc=c:\progs\Embarcadero\XE7

%bcc%\bin\bcc64 -6 -O2 -c -d -u- -DWIN64 sqlite3.c

attrib +r sqlite3.o

pause
