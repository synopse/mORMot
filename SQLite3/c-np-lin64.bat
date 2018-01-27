@rem Use NewPascal cross-compiler to compile our own patched sqlite3.c amalgation file
@rem for FPC static linking info fpc-linux64/sqlite3-64.o

set np=c:\np
set bin=%np%\cross\bin\x86_64-linux
set lib=%np%\cross\lib\x86_64-linux
set dlls=%np%\fpcbootstrap\git\mingw32\bin

set gcc=%bin%\x86_64-linux-gcc.exe
set path=%path%;%dlls%;%bin%

@rem echo path

@rem need to create the destination folder only once

mkdir ..\fpc-linux64
@rem need to copy these files only once
copy %lib%\libgcc.a  ..\fpc-linux64

cd ..\fpc-linux64

attrib -r sqlite3-64.o 
del sqlite3-64.o

@rem here we use -O1 since -O2 triggers unexpected GPF :(
%gcc% -c -O1 -static -DSQLITE_ENABLE_FTS3 -DNDEBUG -DNO_TCL -D_CRT_SECURE_NO_DEPRECATE -DSQLITE_TEMP_STORE=1 -m64 -I. ..\SQLite3\sqlite3.c -o sqlite3-64.o
rem -ldl -lpthread -lc 

attrib +r sqlite3-64.o 

pause
