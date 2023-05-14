dnl Autoconf configure script for Dynamo
dnl Copyright (C) 2011-2023 Stephane Carrez
dnl Written by Stephane Carrez (Stephane.Carrez@gmail.com)
dnl
dnl  Licensed under the Apache License, Version 2.0 (the "License");
dnl  you may not use this file except in compliance with the License.
dnl  You may obtain a copy of the License at
dnl
dnl     http://www.apache.org/licenses/LICENSE-2.0
dnl
dnl  Unless required by applicable law or agreed to in writing, software
dnl  distributed under the License is distributed on an "AS IS" BASIS,
dnl  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
dnl  See the License for the specific language governing permissions and
dnl  limitations under the License.
dnl 
dnl Process this file with autoconf to produce a configure script.

AC_INIT
AC_CONFIG_SRCDIR([src/gen.ads])

# Current release settings
GEN_MAJOR_VERSION=1
GEN_MINOR_VERSION=4
GEN_MICRO_VERSION=0
GEN_VERSION=$GEN_MAJOR_VERSION.$GEN_MINOR_VERSION.$GEN_MICRO_VERSION

AM_GNAT_LIBRARY_PROJECT

# checking for local tools
AC_PROG_CC
AC_CHECK_PROG(UNZIP_CHECK,unzip,yes)
if test x"$UNZIP_CHECK" != x"yes" ; then
    AC_MSG_ERROR([Please install unzip before installing Dynamo.])
fi

# Set the version number of Dynamo
AC_SUBST(GEN_VERSION)
AC_SUBST(GEN_MAJOR_VERSION)
AC_SUBST(GEN_MINOR_VERSION)
AC_SUBST(GEN_MICRO_VERSION)

GEN_LIBDIR="lib"
AC_SUBST(GEN_LIBDIR)

AC_MSG_CHECKING([for GNAT project paths])
if test ${prefix} = "NONE"; then
   gnat_project_paths=/usr/share/gpr:/usr/share/ada/adainclude
else
   gnat_project_paths=${prefix}/share/gpr:${prefix}/share/ada/adainclude
fi
ac_gprbuild=`which gprbuild`
ac_gprbuild_dir=`dirname $ac_gprbuild`
ac_gnat_share_dir=`realpath ${ac_gprbuild_dir}/../share/gpr`
if test -d $ac_gnat_share_dir; then
   gnat_project_paths="${gnat_project_paths}:${ac_gnat_share_dir}"
fi
ac_gnat_share_dir=`realpath ${ac_gprbuild_dir}/../share/adainclude`
if test -d $ac_gnat_share_dir; then
   gnat_project_paths=${gnat_project_paths}:${ac_gnat_share_dir}
fi
AC_MSG_RESULT(${gnat_project_paths})

AC_ARG_WITH(gprpath,
    AS_HELP_STRING([--with-gprpath=paths], [Paths for GNAT projects]),
    [
      gnat_project_paths=${withval}
    ],
    [
    ])
GNAT_PROJECT_PATHS=${gnat_project_paths}
AC_SUBST(GNAT_PROJECT_PATHS)

# directories GNAT project gives the installation paths
AM_GNAT_CHECK_PROJECT([gnatprj],[gnatprj])

if test T$ac_cv_gnat_project_gnatprj = Tno; then

AM_GNAT_CHECK_PROJECT([gpr],[gpr])
if test T$ac_cv_gnat_project_gpr = Tno; then
  WITH_GPR='';
  DYNAMO_HAVE_GPR='no';
else
  WITH_GPR="with \"${GPR_DIR}gpr\";";
  DYNAMO_HAVE_GPR='yes';
fi

else
  WITH_GPR="with \"${GPR_DIR}gnatprj\";";
  DYNAMO_HAVE_GPR='yes';
fi

AC_SUBST(DYNAMO_HAVE_GPR)
AC_SUBST(WITH_GPR)

AM_GNAT_FIND_ADA_UTIL([Dynamo])
AM_GNAT_FIND_ADA_EL([Dynamo])
AM_GNAT_FIND_ADA_SECURITY([Dynamo])
AM_GNAT_FIND_ADA_SERVLET([Dynamo])
AM_GNAT_FIND_ADA_SERVER_FACES([Dynamo])
AM_GNAT_FIND_ADA_ADO([Dynamo])

if test x"$ASF_DIR" = x; then
  ac_asf_dir=${prefix}/share/dynamo/asf
else
  ac_asf_dir=`realpath $ASF_DIR`
fi

BUNDLE_DIR='#{generator_config_dir}/../asf/bundles'
BUNDLE_DIR="$BUNDLE_DIR;$ac_asf_dir/bundles"
AC_SUBST(BUNDLE_DIR)

DYNAMO_SEARCH_DIR='#{generator_config_dir}'
AC_SUBST(DYNAMO_SEARCH_DIR)

AC_SUBST(mkdir_p)

AC_CONFIG_FILES([Makefile])
AC_CONFIG_FILES([dynamo.gpr
dynamo_tests.gpr
regtests/check_build/check_build.gpr
config/generator.properties
])
AC_OUTPUT
