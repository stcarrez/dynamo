#!/bin/sh
NAME=dynamo.cov
lcov --base-directory . --directory . -c --include "*/dynamo/src/*" -o $NAME
lcov --remove $NAME "*/src/gpr/*" -o $NAME
lcov --remove $NAME "*/src/gnat/*" -o $NAME
lcov --remove $NAME "*/src/yaml/*" -o $NAME
rm -rf cover
genhtml --ignore-errors source -o ./cover -t "test coverage" --num-spaces 4 $NAME
 
