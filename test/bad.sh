#!/bin/bash

passed=0
tests=0

for FILE in ./examples/bad/*.lat
do
    ((tests++))
    NAME=${FILE##*/}
    if [[ ${NAME:0:3} = "stx" ]];
    then
        if cat $FILE | ./dist-newstyle/build/x86_64-linux/ghc-8.10.7/latte-0.1.0.0/x/test/build/test/test | grep -q "Parse              Failed...";
        then
        ((passed++))
        else
            echo "$NAME -"
        fi
    else
        if cmp --silent -- "${FILE%.*}.out" <(./dist-newstyle/build/x86_64-linux/ghc-8.10.7/latte-0.1.0.0/x/app/build/app/app $FILE);
        then
            ((passed++))
        else
            echo "$NAME -"
        fi
    fi
done

printf "\n\nPassed: $passed / $tests\n"
