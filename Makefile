# You may edit this makefile as long as you keep these original 
# target names defined.
BUILD=debug
NAME=dynamo
VERSION=1.5.0

DIST_DIR=dynamo-$(VERSION)
DIST_FILE=dynamo-$(VERSION).tar.gz

MAKE_ARGS += -XDYNAMO_BUILD=$(BUILD)

-include Makefile.conf

include Makefile.defaults

INSTALL = /usr/bin/install -c

srcdir = .
top_srcdir = .

exec_prefix = ${prefix}

bindir = ${exec_prefix}/bin
sbindir = ${exec_prefix}/sbin
libexecdir = ${exec_prefix}/libexec
datadir = ${datarootdir}
datarootdir= ${prefix}/share
sysconfdir = ${prefix}/etc
sharedstatedir = ${prefix}/com
localstatedir = ${prefix}/var
libdir = ${exec_prefix}/lib
infodir = ${datarootdir}/info
mandir = ${datarootdir}/man
includedir = ${prefix}/include

top_builddir = .
config_dir=${datadir}/dynamo

MKDIR=mkdir
CP=cp

# Build executables for all mains defined by the project.
build::	local-build build-tests

local-build:
	$(BUILD_COMMAND) $(GPRFLAGS) $(MAKE_ARGS)

build-tests:
	cd regtests && $(BUILD_COMMAND) $(GPRFLAGS) $(MAKE_ARGS)

bin/dynamo:  local-build
bin/dynamo_harness:  bin/dynamo build-tests

# Clean the root project of all build products.
clean::
	-rm -rf lib obj bin/dynamo bin/dynamo_harness

dist-clean:: clean
	-rm -f config.status *.log *~

# Clean, then build executables for all mains defined by the project.
rebuild: clean build

doc::
	$(DYNAMO) build-doc -markdown wiki

check test:	 build
	-rm -rf test-app
	-rm -f dynamo-junit.xml
	chmod +x regtests/support/yui-compressor
	chmod +x regtests/support/pngcrush
	# The test 'Build generated model files (UML)' can take more than 1 minute.
	export PATH=`pwd`/regtests/support:$$PATH ; \
	bin/dynamo_harness -t 120 -xml dynamo-aunit.xml

GENERATOR=dynamo

generate:
	$(GENERATOR) generate config/db
	rm -f src/model/gen-database-model.adb

install:: install-data

install-data::	  install_dirs
	$(INSTALL) bin/dynamo $(DESTDIR)$(prefix)/bin/dynamo
	$(INSTALL) bin/argouml.sh $(DESTDIR)$(prefix)/bin/argouml
	$(INSTALL) man/man1/dynamo.1 $(DESTDIR)$(prefix)/share/man/man1/dynamo.1
	$(CP) config/*.jar $(DESTDIR)$(prefix)/share/dynamo/base
	$(CP) config/*.properties $(DESTDIR)$(prefix)/share/dynamo/base
	$(CP) config/*.xsl $(DESTDIR)$(prefix)/share/dynamo/base
	$(CP) config/*.sql $(DESTDIR)$(prefix)/share/dynamo/base
	-rm -rf $(DESTDIR)$(prefix)/share/dynamo/base/templates
	-rm -rf $(DESTDIR)$(prefix)/share/dynamo/base/commands
	-rm -rf $(DESTDIR)$(prefix)/share/dynamo/base/mappings
	-rm -rf $(DESTDIR)$(prefix)/share/dynamo/base/uml
	$(CP) -r config/uml $(DESTDIR)$(prefix)/share/dynamo/base
	$(CP) -r config/mappings $(DESTDIR)$(prefix)/share/dynamo/base
	$(CP) -r config/templates $(DESTDIR)$(prefix)/share/dynamo/base
	$(CP) -r config/commands $(DESTDIR)$(prefix)/share/dynamo/base
	$(CP) config/db/*.xml $(DESTDIR)$(prefix)/share/dynamo/base/db

install_dirs:
	${MKDIR} -p $(DESTDIR)${bindir}
	${MKDIR} -p $(DESTDIR)${prefix}/share/dynamo/base
	${MKDIR} -p $(DESTDIR)${prefix}/share/dynamo/base/db
	${MKDIR} -p $(DESTDIR)${prefix}/share/dynamo/base/mappings
	${MKDIR} -p $(DESTDIR)${prefix}/share/dynamo/base/commands
	${MKDIR} -p $(DESTDIR)${prefix}/share/man/man1

uninstall::
	rm -rf $(DESTDIR)${prefix}/share/dynamo/base
	rm -f $(DESTDIR)${bindir}/dynamo
	rm -f $(DESTDIR)${bindir}/argouml
	rm -f $(DESTDIR)$(prefix)/share/man/man1/dynamo.1

.PHONY: doc
