@rem Use MINGW to compile our version of sqlite3.c amalgation file for FPC compatibility

set path=%path%;c:\progs\mingw\bin

del sqlite3.o
del sqlite3fts3.o
gcc -O2 -c -DSQLITE_ENABLE_FTS3 -DSQLITE_ENABLE_FTS4 -DSQLITE_ENABLE_FTS3_PARENTHESIS sqlite3.c
copy sqlite3.o sqlite3fts3.o

pause
