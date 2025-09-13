# Dynamo Ada Generator

[![Alire](https://img.shields.io/endpoint?url=https://alire.ada.dev/badges/dynamo.json)](https://alire.ada.dev/crates/dynamo)
[![Ada 2012](https://img.shields.io/badge/2012-inside-green?logo=ada&logoColor=white&logoSize=auto)](https://adaic.org/ada-resources/standards/ada12)
[![Build Status](https://img.shields.io/endpoint?url=https://porion.vacs.fr/porion/api/v1/projects/dynamo/badges/build.json)](https://porion.vacs.fr/porion/projects/view/dynamo/summary)
[![Test Status](https://img.shields.io/endpoint?url=https://porion.vacs.fr/porion/api/v1/projects/dynamo/badges/tests.json)](https://porion.vacs.fr/porion/projects/view/dynamo/xunits)
[![Coverage](https://img.shields.io/endpoint?url=https://porion.vacs.fr/porion/api/v1/projects/dynamo/badges/coverage.json)](https://porion.vacs.fr/porion/projects/view/dynamo/summary)
[![Download](https://img.shields.io/badge/download-1.4.0-brightgreen.svg)](https://download.vacs.fr/dynamo/dynamo-1.4.0.tar.gz)
[![License](https://img.shields.io/badge/license-APACHE2-blue.svg)](LICENSE)
[![Commits](https://img.shields.io/github/commits-since/stcarrez/dynamo/1.4.0.svg)](Commits)

This Ada05 application is a code generator used to generate
an Ada Web Application or database mappings from hibernate-like
XML description, YAML doctrine model or UML models.  It provides various commands for the
generation of a web application which uses the Ada Web Application framework
(https://gitlab.com/stcarrez/ada-awa/).

## Version 1.5.0   - Under development
  - Feature #30: Generate support for Alire
  - Feature #33: Allow to customize the list of directories to scan and extract the documentation
  - Feature #34: Add support for beans in the YAML model description
  - New option `-C <dir>` to change directory before executing a command
  - Cleanup build environment to drop configure
  - Fix #32: Generate single quote strings for SQLite schem

## Version 1.4.0   - Aug 2023
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

[List all versions](https://github.com/stcarrez/dynamo/blob/master/NEWS.md)

## Build with Alire

```
alr with dynamo
```

Install it with:
```
make install
```
## Documentation

The Dynamo Ada Generator sources as well as a wiki documentation
is provided on:

   https://gitlab.com/stcarrez/dynamo


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

