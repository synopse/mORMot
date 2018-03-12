@rem Use NewPascal cross-compiler to compile our own patched sqlite3.c amalgation file
@rem for FPC static linking info fpc-darwin32/sqlite3.o

set np=c:\np
set bin=%np%\cross\bin\x86-darwin
set sdk=%np%\cross\lib\x86-darwin\MacOSX10.11.sdk\usr
set lib=%sdk%\lib
set dlls=%np%\fpcbootstrap\git\mingw32\bin

set gcc=%bin%\i386-apple-darwin15-clang.exe
set path=%path%;%dlls%;%bin%
set inc=-I%bin%\include\clang\4.0.1\include -I%sdk%\include

cd ..\static\i386-darwin

attrib -r sqlite3.o 
del sqlite3.o

@rem here we use -O1 since -O2 triggers unexpected GPF :(
%gcc% -c -O1 -m32 -target i386-apple-darwin15 -DSQLITE_ENABLE_FTS3 -DNDEBUG -DNO_TCL -D_CRT_SECURE_NO_DEPRECATE -DSQLITE_TEMP_STORE=1 %inc% -I. ..\..\SQLite3\sqlite3.c -o sqlite3.o

%bin%\i386-apple-darwin15-libtool.exe -static sqlite3.o -o libsqlite3.a

attrib +r sqlite3.o 

pause
