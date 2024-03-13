@echo off
rem Use FpcUpDeluxe cross-compiler toolchain since native GCC generates dependency on newer libc version (e.g. compiled on Ubuntu 20.04 can't link on Ubuntu 18.04).

set FPCARCH=x86_64-linux
set GCCPATH=d:\fpcup\__win\bin\%FPCARCH%

set GCC=%FPCARCH%-gcc
set DST=..\..\static\%FPCARCH%\sqlite3.o
set DST2=..\..\..\lib2\static\%FPCARCH%\sqlite3.o
set path=%path%;%GCCPATH%

del %DST%
del %DST2%
del sqlite3-%FPCARCH%.o
del sqlite3-%FPCARCH%.orig

echo.
echo ---------------------------------------------------
echo Compiling for FPC on %FPCARCH% using %GCC%
%GCC% -static -w -O2 -fno-pic -fno-stack-protector -fomit-frame-pointer -fno-exceptions -fno-asynchronous-unwind-tables -fno-unwind-tables -m64 -DNDEBUG -DNO_TCL -D_CRT_SECURE_NO_DEPRECATE -c sqlite3mc.c -o sqlite3-%FPCARCH%.o

%FPCARCH%-strip -x sqlite3-%FPCARCH%.o

echo Direct use for mORMot 1
copy sqlite3-%FPCARCH%.o %DST%

echo.
echo Renaming dl and pthread symbols for proper cross-GLIBC mORMot 2 execution
copy sqlite3-%FPCARCH%.o sqlite3-%FPCARCH%.orig
rem objconv.exe should be downloaded from https://agner.org/optimize/objconv.zip
objconv -np:pthread:_pthread -np:dl:_dl sqlite3-%FPCARCH%.orig sqlite3-%FPCARCH%.o
del sqlite3-%FPCARCH%.orig

copy sqlite3-%FPCARCH%.o %DST2%

rem pause
