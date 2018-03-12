@rem Use NewPascal cross-compiler to compile our own patched sqlite3.c amalgation file
@rem for FPC static linking info fpc-linux32/sqlite3.o

set np=c:\np
set bin=%np%\cross\bin\i386-linux
set lib=%np%\cross\lib\i386-linux
set dlls=%np%\fpcbootstrap\git\mingw32\bin

set gcc=%bin%\i386-linux-gcc.exe
set path=%path%;%dlls%;%bin%

@rem echo path

cd ..\static\i386-linux

attrib -r sqlite3.o 
del sqlite3.o

%gcc% -c -O2 -static -DNDEBUG -DNO_TCL -D_CRT_SECURE_NO_DEPRECATE -DSQLITE_TEMP_STORE=1 -I. ..\..\SQLite3\sqlite3.c -o sqlite3.o
rem -ldl -lpthread -lc 

attrib +r sqlite3.o 

pause
