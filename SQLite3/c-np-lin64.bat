@rem Use NewPascal cross-compiler to compile our own patched sqlite3.c amalgation file
@rem for FPC static linking info fpc-linux64/sqlite3-64.o

set np=c:\np
set bin=%np%\cross\bin\x86_64-linux
set lib=%np%\cross\lib\x86_64-linux
set dlls=%np%\fpcbootstrap\git\mingw32\bin

set gcc=%bin%\x86_64-linux-gcc.exe
set path=%path%;%dlls%;%bin%

cd ..\static\x86_64-linux

attrib -r sqlite3.o 
del sqlite3.o

@rem here we use -O1 since -O2 triggers unexpected GPF :(
%gcc% -c -O2 -static -DNDEBUG -DNO_TCL -D_CRT_SECURE_NO_DEPRECATE -DSQLITE_TEMP_STORE=1 -m64 -I. ..\..\SQLite3\sqlite3.c -o sqlite3.o
rem -ldl -lpthread -lc 

attrib +r sqlite3.o 

pause
