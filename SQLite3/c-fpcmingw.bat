@rem Use MINGW to compile our version of sqlite3.c amalgation file for FPC compatibility

set mingw=c:\progs\mingw

set path=%path%;%mingw%\bin

@rem need to create the destination folder only once

mkdir ..\fpc-win32
mkdir ..\fpc-linux32
@rem need to copy these files only once
copy %mingw%\lib\libkernel32.a ..\fpc-win32
copy %mingw%\lib\gcc\mingw32\4.8.1\libgcc.a  ..\fpc-win32

cd ..\fpc-win32

attrib -r sqlite3.o 
del sqlite3.o

gcc -O2 -c -DSQLITE_ENABLE_FTS3 -DSQLITE_ENABLE_FTS4 -DSQLITE_ENABLE_FTS3_PARENTHESIS ..\SQLite3\sqlite3.c

attrib +r sqlite3.o 

pause
