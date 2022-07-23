#!/bin/bash

passed=0
tests=0

for FILE in ./examples/good/*.lat
do
    ((tests++))
    NAME=${FILE##*/}
    if cmp --silent -- "${FILE%.*}.out" <(./dist-newstyle/build/x86_64-linux/ghc-8.10.7/latte-0.1.0.0/x/app/build/app/app $FILE);
    then
        ((passed++))
    else
        echo "$NAME -"
    fi
done

printf "\n\nPassed: $passed / $tests\n"
