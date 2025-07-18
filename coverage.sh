#!/bin/sh
NAME=dynamo.cov
alr exec -- lcov --quiet --base-directory . --directory . \
   --no-external  --ignore-errors gcov,unused \
   --exclude '*/<unknown>' \
   --exclude '*/b__*.adb' \
   --exclude '*/samples/*' \
   --exclude '*/regtests*' \
   --exclude '*/gnat/*' \
   --exclude '*/yaml/*' \
   --exclude '*/src/gpr/*' -c -o $NAME
rm -rf cover
genhtml --quiet --ignore-errors source -o ./cover -t "test coverage" --num-spaces 4 $NAME
lcov --summary dynamo.cov
