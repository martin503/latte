#!/bin/bash

passed=0
tests=0

echo "*****GOOD*****"
for FILE in ./examples/good/*.lat
do
    ((tests++))
    NAME=${FILE##*/}
    if cat $FILE | ./dist-newstyle/build/x86_64-linux/ghc-8.10.7/latte-0.1.0.0/x/test/build/test/test | grep -q "Parse Successful!";
    then
        ((passed++))
    else
        echo "$NAME -"
    fi
done

printf "\n*****BAD******\n"
for FILE in ./examples/bad/*.lat
do
    ((tests++))
    NAME=${FILE##*/}
    if cat $FILE | ./dist-newstyle/build/x86_64-linux/ghc-8.10.7/latte-0.1.0.0/x/test/build/test/test | grep -q "Parse Successful!";
    then
        if echo "$NAME" | grep -q "stx";
        then
            echo "$NAME -"
        else
            ((passed++))
        fi
    else
        if echo "$NAME" | grep -q "stx";
        then
            ((passed++))
        else
            echo "$NAME -"
        fi
    fi
done

printf "\n\nPassed: $passed / $tests\n"
