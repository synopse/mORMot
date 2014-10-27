# To compile our version of sqlite3.c amalgation file for FPC compatibility
rm sqlite3.o
rm sqlite3fts3.o
gcc -c sqlite3.c -O2 -ldl -lpthread -lc -DSQLITE_ENABLE_FTS3 -DSQLITE_ENABLE_FTS4 -DSQLITE_ENABLE_FTS3_PARENTHESIS -DSQLITE_ENABLE_RTREE
# to get 32 bit on 64 bit systems
# gcc -c sqlite3.c -O2 -m32 -ldl -lpthread -lc -DSQLITE_ENABLE_FTS3 -DSQLITE_ENABLE_FTS4 -DSQLITE_ENABLE_FTS3_PARENTHESIS -DSQLITE_ENABLE_RTREE
cp sqlite3.o sqlite3fts3.o
gcc -c sqlite3.c -O2 -ldl -lpthread -lc
# to get 32 bit on 64 bit systems
# gcc -c sqlite3.c -O2 -m32 -ldl -lpthread -lc
echo "Done !"
