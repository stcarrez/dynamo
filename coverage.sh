#!/bin/sh
lcov --base-directory . --directory . -c -o dynamo.cov
bin/dynamo_harness -xml dynamo-aunit.xml
lcov --base-directory . --directory . -c -o dynamo.cov
lcov --remove dynamo.cov "/usr*" -o dynamo.cov
lcov --remove dynamo.cov "regtests*" -o dynamo.cov
lcov --remove dynamo.cov "src/gnat/*" -o dynamo.cov
lcov --remove dynamo.cov dynamo/b__dynamo.adb -o dynamo.cov
rm -rf cover
genhtml -o ./cover -t "test coverage" --num-spaces 4 dynamo.cov
 
