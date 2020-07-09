#!/bin/sh

rm -rf ./.resources
./tools/core_res -i ./core_modules/ -o ./.resources/ || exit $1
windres ./.resources/core_res.rc ./core_modules.res