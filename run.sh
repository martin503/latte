#!/bin/bash

if [[ $# != 1 ]]; then
    echo "Wrong number of arguments, usage: $0 <path-to-latte-file>"
    exit 1
fi

./dist-newstyle/build/x86_64-linux/ghc-8.10.7/latte-0.1.0.0/x/app/build/app/app $1
