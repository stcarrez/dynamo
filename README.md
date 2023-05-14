# Dynamo Ada Generator

[![Build Status](https://img.shields.io/jenkins/s/https/jenkins.vacs.fr/Bionic-Dynamo.svg)](https://jenkins.vacs.fr/job/Bionic-Dynamo/)
[![Test Status](https://img.shields.io/jenkins/t/https/jenkins.vacs.fr/Bionic-Dynamo.svg)](https://jenkins.vacs.fr/job/Bionic-Dynamo/)
[![codecov](https://codecov.io/gh/stcarrez/dynamo/branch/master/graph/badge.svg)](https://codecov.io/gh/stcarrez/dynamo)
[![Download](https://img.shields.io/badge/download-1.3.0-brightgreen.svg)](https://download.vacs.fr/dynamo/dynamo-1.3.0.tar.gz)
[![License](https://img.shields.io/badge/license-APACHE2-blue.svg)](LICENSE)
![Commits](https://img.shields.io/github/commits-since/stcarrez/dynamo/1.3.0.svg)

This Ada05 application is a code generator used to generate
an Ada Web Application or database mappings from hibernate-like
XML description, YAML doctrine model or UML models.  It provides various commands for the
generation of a web application which uses the Ada Web Application framework
(https://github.com/stcarrez/ada-awa/).

To build Dynamo, you will need:

* Ada Util     (https://github.com/stcarrez/ada-util          2.5.0)
* Ada EL       (https://github.com/stcarrez/ada-el            1.8.5)
* Ada Security (https://github.com/stcarrez/ada-security      1.4.1)
* Ada Servlet  (https://github.com/stcarrez/ada-servlet       1.6.0)
* Ada Server Faces (https://github.com/stcarrez/ada-asf       1.5.0)
* Ada ADO      (https://github.com/stcarrez/ada-ado           2.3.0)
* XML/Ada      (https://libre.adacore.com/libre/tools/xmlada/  4.4)

## Version 1.4.0   - Under development
- Feature #22: Support to audit creation of new objects in the database
- Feature #23: Update SQL templates for ADO 2.4
- Feature #24: Support for database schema versionning
- Feature #8: Option or configuration to disable generation of <project>.properties file
- Feature #25: Support for queries with 64-bit integer columns
- Feature #26: Plugins without a GNAT project are not always recognized
- Feature #27: UML model evolution for code generation
- Fix #6: Integration test 'Dynamo.Dist' fails
- Fix #28: Dynamo add-module command fails to patch the source
- Fix #29: Single precision float mapped to a double precision in PostgreSQL schema generator

## Version 1.3.0   - Aug 2022
- Fix #5: Generated body does not compile when an enumeration of another UML package is used
- Fix #7: No default type for SQL generation of a column that uses an enumeration
- Fix #9: Option or configuration to disable some SQL generation
- Fix #10: Definition of an UML datatype with a tagged value raises an exception
- Fix #12: Avoid emitting a full qualified type name for types declared in the current package
- Fix #16: Improvement in Markdown documentation generator
- Fix #17: YAML parser: accessibility check failure
- Fix #18: Generate database operation to reload an object
- Fix #19: Add dynamo configuration through environment support
- Fix #20: Give access to the man page from alire
- Fix $21: Generated procedure Create is missing overriding keyword

[List all versions](https://github.com/stcarrez/dynamo/blob/master/NEWS.md)

## Build

Build with the following commands:
```
   ./configure
   make
```
Install it with:
```
   make install
```
## Documentation

The Dynamo Ada Generator sources as well as a wiki documentation
is provided on:

   https://github.com/stcarrez/dynamo


## License

Dynamo integrates some files from the GNU Compiler Collection (5.2.0).
These files are used to read the GNAT project files (see src/gnat).
They are distributed under the GPL version 3 license.

Dynamo integrates the AdaYaml library written by Felix Krause
(but with many fixes) and distributed under the MIT license (see copying.txt and src/yaml).

The Dynamo core implementation is distributed under the Apache 2.0 license.

The Dynamo templates are distributed under the Apache 2.0 license.

The Dynamo generated code can be distributed under whatever license you like.

To help users in using UML modeling together with Dynamo, the [ArgoUML](https://github.com/argouml-tigris-org/argouml)
application is integrated, it is distributed under the Eclipse Public License and the BSD license.

