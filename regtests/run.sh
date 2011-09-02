#!/bin/sh
#
MSG=$1
shift
mkdir -p test-app
cd test-app
TMP='/tmp/dynamo-test.$'
$* 2> $TMP >> tests.log
if test $? -ne 0; then
   ERROR=`cat $TMP`
   set -x
   add-junit-result.sh ../dynamo-junit.xml -name dynamo -error "$ERROR" "$MSG"
else
   add-junit-result.sh ../dynamo-junit.xml -name dynamo "$MSG"
fi
rm -f /tmp/dynamo-test.$
