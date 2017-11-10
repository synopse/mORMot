#!/bin/bash

rm -rf ./fpc/*
mkdir -p ./fpc/lib/x86_64-linux
mkdir -p ./fpc/bin/x86_64-linux

fpc -B -MObjFPC -Scagi -Cg -Cirot -gw2 -gl -l -dFPCSQLITE3STATIC -dUseCThreads -Fi -Fifpc/lib/x86_64-linux -Fi.. -Fl../fpc-linux64 -Fu.. -FuDDD/dom -FuDDD/infra "-FuSamples/33 - ECC" -Fu. -FUfpc/lib/x86_64-linux -FEfpc/bin/x86_64-linux/ ./TestSQL3.dpr
