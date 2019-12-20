@echo off

attrib -r sqlite3.o 
del sqlite3.o

set bcc=d:\Dev\bcc64ce

%bcc%\bin\bcc64 -isystem "%bcc%\include" -isystem "%bcc%\include\windows\sdk" -isystem "%bcc%\include\dinkumware64" -isystem "%bcc%\include\windows\crtl" -O2 -c -DWIN64 sqlite3.c

attrib +r sqlite3.o

pause
