#!/bin/sh
INSTALL_DIR=`pwd`
gnatprep -DCONFIG_DIR=\"${INSTALL_DIR}/config\" -DVERSION=\"1.2.3\" src/gen-configs.gpb src/gen-configs.ads
