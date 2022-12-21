#!/bin/sh
INSTALL_DIR=`pwd`
gnatprep -DCONFIG_DIR=\"${INSTALL_DIR}/config\" -DVERSION=\"1.4.0\" src/gen-configs.gpb src/gen-configs.ads
GNATBUILD=`which gprbuild`
GNATBIN=`dirname ${GNATBUILD}`
GNATDIR=`dirname ${GNATBIN}`
GNAT_PROJECT_PATHS="${GNATDIR}/share/gpr"
sed -e "s,@GNAT_PROJECT_PATHS@,${GNAT_PROJECT_PATHS}," \
    -e 's,@DYNAMO_SEARCH_DIR@,.,' \
    -e "s,@BUNDLE_DIR@,.," \
    config/generator.properties.in > config/generator.properties
