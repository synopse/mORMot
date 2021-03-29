#!/bin/sh

ARCH=i386-android

CROSS=/home/ab/fpcup/cross/bin/all-android
GCC=$CROSS/bin/clang
DST=../../static/$ARCH/sqlite3.o
DST2=../../../lib2/static/$ARCH/sqlite3.o

rm $DST
rm $DST2
rm sqlite3-$ARCH.o

echo
echo ---------------------------------------------------
echo Compiling for FPC on $ARCH using $GCC
$GCC --target=i686-linux-androideabi21 -static -Wno-pointer-sign -O2 -DNDEBUG -DNO_TCL -D_CRT_SECURE_NO_DEPRECATE -D__ARM_PCS_VFP -c sqlite3mc.c -o sqlite3-$ARCH.o

#$CROSS/bin/llvm-strip sqlite3-$ARCH.o
# strip blows all external symbols

cp sqlite3-$ARCH.o $DST
cp sqlite3-$ARCH.o $DST2

