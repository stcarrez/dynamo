#!/bin/sh
INSTALL_DIR=`pwd`
gnatprep -DCONFIG_DIR=\"${INSTALL_DIR}/config\" -DVERSION=\"1.2.3\" src/gen-configs.gpb src/gen-configs.ads
sed -e 's,@GNAT_PROJECT_PATHS@,.,' \
    -e 's,@DYNAMO_SEARCH_DIR@,.,' \
    -e "s,@BUNDLE_DIR@,.," \
    config/generator.properties.in > config/generator.properties