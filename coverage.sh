#!/bin/sh
NAME=dynamo.cov
lcov --quiet --base-directory . --directory . -c --include "*/dynamo/src/*" -o $NAME
lcov --quiet --remove $NAME "*/src/gpr/*" -o $NAME
lcov --quiet --remove $NAME "*/src/gnat/*" -o $NAME
lcov --quiet --remove $NAME "*/src/yaml/*" -o $NAME
rm -rf cover
genhtml --quiet --ignore-errors source -o ./cover -t "test coverage" --num-spaces 4 $NAME
 
