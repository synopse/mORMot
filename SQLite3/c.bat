@echo off

attrib -r sqlite3.obj
del sqlite3.obj

rem set bcc=\dev\bccXE7
set bcc=d:\dev\bcc

%bcc%\bin\bcc32 -6 -O2 -c -d -u- sqlite3.c

attrib +r sqlite3.obj

pause
