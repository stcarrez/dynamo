# Dynamo Ada Generator

[![Build Status](https://img.shields.io/jenkins/s/https/jenkins.vacs.fr/Bionic-Dynamo.svg)](https://jenkins.vacs.fr/job/Bionic-Dynamo/)
[![Test Status](https://img.shields.io/jenkins/t/https/jenkins.vacs.fr/Bionic-Dynamo.svg)](https://jenkins.vacs.fr/job/Bionic-Dynamo/)
[![codecov](https://codecov.io/gh/stcarrez/dynamo/branch/master/graph/badge.svg)](https://codecov.io/gh/stcarrez/dynamo)
[![Download](https://img.shields.io/badge/download-0.9.0-brightgreen.svg)](https://download.vacs.fr/dynamo/dynamo-0.9.0.tar.gz)
[![License](https://img.shields.io/badge/license-APACHE2-blue.svg)](LICENSE)
![Commits](https://img.shields.io/github/commits-since/stcarrez/dynamo/0.9.0.svg)

This Ada05 application is a code generator used to generate
an Ada Web Application or database mappings from hibernate-like
XML description, YAML doctrine model or UML models.  It provides various commands for the
generation of a web application which uses the Ada Web Application framework
(https://github.com/stcarrez/ada-awa/).

To build Dynamo, you will need:

* Ada Util     (https://github.com/stcarrez/ada-util          1.9.0)
* Ada EL       (https://github.com/stcarrez/ada-el            1.6.1)
* Ada Security (https://github.com/stcarrez/ada-security      1.2.0)
* Ada Servlet  (https://github.com/stcarrez/ada-servlet       1.2.0)
* Ada Server Faces (https://github.com/stcarrez/ada-asf       1.2.0)
* Ada ADO      (https://github.com/stcarrez/ada-ado           1.2.0)
* XML/Ada      (https://libre.adacore.com/libre/tools/xmlada/  4.3)

Build with the following commands:
```
   ./configure
   make
```
Install it with:
```
   make install
```
# Documentation

The Dynamo Ada Generator sources as well as a wiki documentation
is provided on:

   https://github.com/stcarrez/dynamo


# License

Dynamo integrates some files from the GNU Compiler Collection (5.2.0).
These files are used to read the GNAT project files (see src/gnat).
They are distributed under the GPL version 3 license.

Dynamo integrates the AdaYaml library written by Felix Krause and
distributed under the MIT license (see copying.txt and src/yaml).

The Dynamo core implementation is distributed under the Apache 2.0 license.

The Dynamo templates are distributed under the Apache 2.0 license.

The Dynamo generated code can be distributed under whatever license you like.
