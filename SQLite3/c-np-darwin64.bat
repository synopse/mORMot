@rem Use NewPascal cross-compiler to compile our own patched sqlite3.c amalgation file
@rem for FPC static linking info fpc-darwin64/sqlite3.o

set np=c:\np
set bin=%np%\cross\bin\x86-darwin
set sdk=%np%\cross\lib\x86-darwin\MacOSX10.11.sdk\usr
set lib=%sdk%\lib
set dlls=%np%\fpcbootstrap\git\mingw32\bin

set gcc=%bin%\x86_64-apple-darwin15-clang.exe
set path=%path%;%dlls%;%bin%
set inc=-I%bin%\include\clang\5.0.1\include -I%sdk%\include

@rem echo path

attrib -r ..\static\x86_64-darwin\sqlite3.o 
del ..\static\x86_64-darwin\sqlite3.o

@rem may try to use -O1 if -O2 triggers unexpected GPF :(
%gcc% -c -O2 -m64 -target x86_64-apple-darwin15 -DSQLITE_ENABLE_FTS3 -DNDEBUG -DNO_TCL -D_CRT_SECURE_NO_DEPRECATE -DSQLITE_TEMP_STORE=1 %inc% -I. sqlite3.c -o ..\static\x86_64-darwin\sqlite3.o

cd ..\static\x86_64-darwin

%bin%\x86_64-apple-darwin15-libtool.exe -static sqlite3.o -o libsqlite3.a

attrib +r sqlite3.o 

pause
