@rem Use NewPascal cross-compiler to compile our own patched sqlite3.c amalgation file
@rem for FPC static linking info fpc-linuxarm/sqlite3.o

set np=c:\np
set bin=%np%\cross\bin\arm-linux
set lib=%np%\cross\lib\arm-linux
set dlls=%np%\fpcbootstrap\git\mingw32\bin

set gcc=%bin%\arm-linux-gnueabihf-gcc.exe
set path=%path%;%dlls%;%bin%

cd ..\static\arm-linux

attrib -r sqlite3.o 
del sqlite3.o

@rem here we use -O1 since -O2 triggers unexpected GPF :(
%gcc% -c -O1 -static -DNDEBUG -DNO_TCL -D_CRT_SECURE_NO_DEPRECATE -DSQLITE_TEMP_STORE=1 -D__ARM_PCS_VFP -v -mfloat-abi=hard -U_HAVE_MINGW_H -U_HAVE__MINGW_H -I%bin%\include -I. ..\..\SQLite3\sqlite3.c -o sqlite3.o
rem -ldl -lpthread -lc 

attrib +r sqlite3.o 

pause
