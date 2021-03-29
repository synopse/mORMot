#!/bin/sh

ARCH=aarch64-android

CROSS=/home/ab/fpcup/cross/bin/all-android/bin
GCC=$CROSS/clang
DST=../../static/$ARCH/sqlite3.o
DST2=../../../lib2/static/$ARCH/sqlite3.o

rm $DST
rm $DST2
rm sqlite3-$ARCH.o

echo
echo ---------------------------------------------------
echo Compiling for FPC on $ARCH using $GCC
$GCC --target=aarch64-linux-android21 -static -Wno-pointer-sign -O2 -DNDEBUG -DNO_TCL -D_CRT_SECURE_NO_DEPRECATE -c sqlite3mc.c -o sqlite3-$ARCH.o

#$CROSS/llvm-strip sqlite3-$ARCH.o
# striping remove all exported symbols :(

cp sqlite3-$ARCH.o $DST
cp sqlite3-$ARCH.o $DST2

