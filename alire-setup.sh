#!/bin/sh
INSTALL_DIR=`pwd`/config
echo "ROOT=${CRATE_ROOT}"
gnatprep -DCONFIG_DIR=\"${INSTALL_DIR}/config\" -DVERSION=\"1.2.3\" src/gen-configs.gpb src/gen-configs.ads
