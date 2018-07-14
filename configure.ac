dnl Autoconf configure script for Dynamo
dnl Copyright (C) 2011-2018 Stephane Carrez
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

AC_INIT(src/gen.ads)

AC_CANONICAL_HOST
AC_EXEEXT

# Current release settings
GEN_MAJOR_VERSION=0
GEN_MINOR_VERSION=9
GEN_MICRO_VERSION=0
GEN_VERSION=$GEN_MAJOR_VERSION.$GEN_MINOR_VERSION.$GEN_MICRO_VERSION

AC_MSG_CHECKING([number of processors])
NR_CPUS=`getconf _NPROCESSORS_CONF 2>/dev/null || getconf _NPROCESSORS_CONF 2>/dev/null || echo 1`
AC_MSG_RESULT($NR_CPUS)
AC_SUBST(NR_CPUS)

AM_GNAT_CHECK_GPRBUILD

# checking for local tools
AC_PROG_CC
AC_PROG_MAKE_SET
AC_PROG_INSTALL
AC_PROG_RANLIB
AC_PROG_LN_S
AC_CHECK_PROG(UNZIP_CHECK,unzip,yes)
if test x"$UNZIP_CHECK" != x"yes" ; then
    AC_MSG_ERROR([Please install unzip before installing Dynamo.])
fi

# Set the version number of Dynamo
AC_SUBST(GEN_VERSION)
AC_SUBST(GEN_MAJOR_VERSION)
AC_SUBST(GEN_MINOR_VERSION)
AC_SUBST(GEN_MICRO_VERSION)

EXEC_PREFIX="$prefix"
AC_SUBST(EXEC_PREFIX)

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
AM_GNAT_CHECK_PROJECT([gpr],[gpr])
UTIL_HAVE_PRJ_DIRECTORIES=${gnat_project_gpr}
AC_SUBST(UTIL_HAVE_PRJ_DIRECTORIES)

##########################################
# Ada Util library
##########################################
AM_GNAT_FIND_PROJECT([gpr],[GNAT Project Library],[gpr],
  [],
  [Building Dynamo requires the GNAT Project Library.],
  [
    GPR_DIR=${gnat_project_dir_gpr}
  ])
AC_SUBST(GPR_DIR)

##########################################
# Ada Util library
##########################################
AM_GNAT_FIND_PROJECT([ada-util],[Ada Utility Library],[util],
  [git@github.com:stcarrez/ada-util.git],
  [Building Dynamo requires the Ada Utility Library.],
  [
    UTIL_DIR=${gnat_project_dir_util}
  ])
AC_SUBST(UTIL_DIR)

##########################################
# Ada Yaml
##########################################
#AM_GNAT_FIND_PROJECT([yaml],[AdaYaml Library],[yaml],
#  [git@github.com:yaml/AdaYaml.git],
#  [Building Dynamo requires the Ada Yaml Library.],
#  [
#    YAML_DIR=${gnat_project_dir_yaml}
#  ])
#AC_SUBST(YAML_DIR)

##########################################
# Ada Expression Language library
##########################################
AM_GNAT_FIND_PROJECT([ada-el],[Ada Expression Language Library],[el],
  [git@github.com:stcarrez/ada-el.git],
  [Building Dynamo requires the Ada EL Library.],
  [
    EL_DIR=${gnat_project_dir_el}
  ])
AC_SUBST(EL_DIR)

##########################################
# Ada Server Faces library
##########################################
AM_GNAT_FIND_PROJECT([ada-asf],[Ada Server Faces],[asf],
  [git@github.com:stcarrez/ada-asf.git],
  [Building Dynamo requires the Ada Server Faces Library.],
  [
    ASF_DIR=${gnat_project_dir_asf}
  ])
AC_SUBST(ASF_DIR)

##########################################
# Ada Security library
##########################################
AM_GNAT_FIND_PROJECT([ada-security],[Ada Security Library],[security],
  [git@github.com:stcarrez/ada-security.git],
  [Building Dynamo requires the Ada Security Library.],
  [
    SECURITY_DIR=${gnat_project_dir_security}
  ])
AC_SUBST(SECURITY_DIR)

##########################################
# Ada Servlet library
##########################################
AM_GNAT_FIND_PROJECT([ada-servlet],[Ada Servlet Library],[servlet_core],
  [git@github.com:stcarrez/ada-servlet.git],
  [Building Dynamo requires the Ada Servlet Library.],
  [
    SERVLET_DIR=${gnat_project_dir_servlet_core}
  ])
AC_SUBST(SERVLET_DIR)

##########################################
# Ada Database Objects library
##########################################
AM_GNAT_FIND_PROJECT([ada-ado],[Ada Database Objects],[ado],
  [git@github.com:stcarrez/ada-ado.git],
  [Building Dynamo requires the Ada Database Objects Library.],
  [
    ADO_DIR=${gnat_project_dir_ado}
  ])
AC_SUBST(ADO_DIR)

AM_GNAT_CHECK_PROJECT([ado_mysql],[${ADO_DIR}ado_mysql])
if test T$gnat_project_ado_mysql = Tno; then
  WITH_ADO_MYSQL='';
  DYNAMO_HAVE_MYSQL='no';
else
  WITH_ADO_MYSQL="with \"${ADO_DIR}ado_mysql\";";
  DYNAMO_HAVE_MYSQL='yes';
fi
AC_SUBST(DYNAMO_HAVE_MYSQL)
AC_SUBST(WITH_ADO_MYSQL)

AM_GNAT_CHECK_PROJECT([ado_sqlite],[${ADO_DIR}ado_sqlite])
if test T$gnat_project_ado_sqlite = Tno; then
  WITH_ADO_SQLITE='';
  DYNAMO_HAVE_SQLITE='no';
else
  WITH_ADO_SQLITE="with \"${ADO_DIR}ado_sqlite\";";
  DYNAMO_HAVE_SQLITE='yes';
fi
AC_SUBST(DYNAMO_HAVE_SQLITE)
AC_SUBST(WITH_ADO_SQLITE)

AC_SUBST(mkdir_p)

AM_UTIL_CHECK_INSTALL

AC_CONFIG_FILES([Makefile])
AC_OUTPUT(
dynamo.gpr
dynamo_tests.gpr
regtests/check_build/check_build.gpr
config/generator.properties
)
