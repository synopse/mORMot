#!/bin/sh

ARCH=x86_64-darwin
DST=../../static/$ARCH/sqlite3.o
DST2=../../../lib2/static/$ARCH/sqlite3.o

#CROSS=/home/ab/fpcup/cross
# use older but working fpcupdeluxe cross compiler
CROSS=/home/abouchez/fpcupdeluxe/__darwin
SDK=$CROSS/lib/x86-darwin/MacOSX10.11.sdk\usr
GCC=$CROSS/bin/x86-darwin/x86_64-apple-darwin15

rm $DST
rm $DST2
rm sqlite3-$ARCH.o

echo
echo ---------------------------------------------------
echo Compiling for FPC on $ARCH using $GCC
$GCC-clang -static -target x86_64-apple-darwin15 -O2 -m64 -Wno-pointer-sign -DNDEBUG -DNO_TCL -D_CRT_SECURE_NO_DEPRECATE -I$SDK/include -c sqlite3mc.c -o sqlite3-$ARCH.o
cp sqlite3-$ARCH.o $DST
cp sqlite3-$ARCH.o $DST2

$GCC-libtool -static sqlite3-$ARCH.o -o ../../static/$ARCH/libsqlite3.a
$GCC-libtool -static sqlite3-$ARCH.o -o ../../../lib2/static/$ARCH/libsqlite3.a
