#!/bin/bash

echo DescFile=$1
echo Push=$2
echo FossilRepository=$3


cd $3
fossil ci -M --no-warnings $1

# push if chkFossilPush checked in SourceCodeRep tool
if [ $2 -eq 1 ]
then
  fossil --no-warnings push
fi