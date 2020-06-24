@echo off

attrib -r ..\sqlite3.obj
del ..\sqlite3.obj

set bcc=d:\dev\DelphiXE7
rem set bcc=d:\dev\bcc

echo ---------------------------------------------------
echo Compiling for Delphi Win32 using %bcc%

%bcc%\bin\bcc32 -6 -Oi -O2 -c -d -u- sqlite3mc.c
copy sqlite3mc.obj ..\sqlite3.obj
del sqlite3mc.obj

attrib +r ..\sqlite3.obj

rem pause
