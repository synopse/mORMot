#!/bin/bash

echo FossilRepository=$1
echo GitRepository=$2
echo GitExe=$3
echo DescFile=$4
echo DevPath=$5

echo
echo
echo mORMot repository
echo -----------------

cd $2
$3 pull

rsync -av --update --existing $1/* $2
#rsync -av --update --existing $5/*.o* $2

$3 add .
$3 commit -a --file=$4
$3 push
