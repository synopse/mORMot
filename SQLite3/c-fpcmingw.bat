@rem Use MINGW to compile our version of sqlite3.c amalgamation file for FPC compatibility

set mingwvers=5.2.0
rem set mingw=c:\progs\mingw\i686-%mingwvers%-posix-dwarf-rt_v4-rev0\mingw32
set mingw=c:\progs\msys64\mingw32

set path=%path%;%mingw%\bin

cd ..\static\i386-win32

set src=..\..\SQLite3\

attrib -r sqlite3.o 
del sqlite3.o

gcc -O2 -DWIN32 -DNDEBUG -D_WINDOWS -c %src%sqlite3.c

attrib +r sqlite3.o 

pause
