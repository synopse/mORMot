@rem Use MINGW64 to compile sqlite3.c amalgamation file for FPC compatibility

set mingwvers=7.0.0
set mingw=c:\progs\msys64\mingw64

set path=%path%;%mingw%\bin

@rem echo path

cd ..\static\x86_64-win64

set src=..\..\SQLite3\

attrib -r sqlite3.o 
del sqlite3.o
attrib -r sqlite3-64.dll
del sqlite3-64.dll
attrib -r %src%exe\sqlite3-64.dll
del %src%exe\sqlite3-64.dll

gcc -O2 -shared -DSQLITE_MMAP_READWRITE -DSQLITE_ENABLE_RTREE=1 -DSQLITE_ENABLE_FTS3 -DSQLITE_ENABLE_FTS4  -DDSQLITE_ENABLE_FTS5 -DSQLITE_ENABLE_FTS3_PARENTHESIS -DSQLITE_ENABLE_JSON1 -DSQLITE_ENABLE_DESERIALIZE -DWIN64 -DNDEBUG -D_WINDOWS -D_USRDLL -DNO_TCL -D_CRT_SECURE_NO_DEPRECATE -DSQLITE_MAX_EXPR_DEPTH=0 -DSQLITE_THREADSAFE=1 -DTEMP_STORE=1 -m64 -I. %src%amalgamation\sqlite3.c -o sqlite3-64.dll -Wl,--out-implib,libsqlite3-64.a

gcc -c -O2 -static -DWIN64 -DNDEBUG -D_WINDOWS -DNO_TCL -D_CRT_SECURE_NO_DEPRECATE -DSQLITE_TEMP_STORE=1 -m64 -I. %src%sqlite3.c -o sqlite3.o

@rem -fno-stack-check -fno-stack-protector -mno-stack-arg-probe 

attrib +r sqlite3.o 

copy sqlite3-64.dll %src%exe
attrib +r %src%exe\sqlite3-64.dll
del sqlite3-64.dll 

pause
