Version 1.4.0   - Under development
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

Version 1.3.0   - Aug 2022
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
  - Fix #21: Generated procedure Create is missing overriding keyword

Version 1.2.2   - Jul 2021
  - Fix the SQL type definition for double on PostgreSQL
  - Fix double support and nullable entity_type
  - Fix SQL generation for a foreign key with variable length

Version 1.2.1   - Feb 2021
  - Fix uninitialized float values in generated code

Version 1.2     - August 2020
  - Integrate ArgoUML 0.35.2-2020-07-05
  - Fix SQL generation with 'auto' generator
  - Fix XML Hibernate mapping support
  - Improvement in SQL schema generation

Version 1.1     - June 2020
  - Fix compilation with GNAT 2020
  - Integrate ArgoUML 0.35.2-2020-06-20

Version 1.0     - May 2020
  - Improvement and fixes in the YAML database model files
  - Add support for Nullable_String type
  - Generate Postgresql SQL files from the model files
  - Add support for database record auditing
  - Add support for floating point
  - Add support for CSS and Javascript merge in the dist command

Version 0.9     - Jul 2018
  - New type ASF.Parts.Part in the Dynamo UML model
  - Add support to generate ASF Upload method in UML Ada beans
  - Add support to generate AWA event actions
  - Generate JSON/XML serialization code for UML classes
  - Update the 'create-database' command to support SQLite
  - Fix model generation for multiple primary keys per table
  - Add support for <exclude> patterns in the dist command
  - Add support for YAML database model files

Version 0.8     - Dec 2015
  - Support to generate Markdown documentation
  - Support to generate query Ada bean operations
  - Better code generation and support for UML Ada beans

Version 0.7.1   - Jul 2014
  - Fix minor configuration issue with GNAT 2014

Version 0.7.0   - May 2014
  - New project template to generate Gtk Ada application
  - Register the new module in the application when they are added
  - Update the current testsuite when new tests are added
  - New stereotype <<Limited_Bean>> for Ada bean generation
  - Support for the creation of Debian packages
  - New command 'add-form' and 'add-module-operation'

Version 0.6.0   - Feb 2013
  - New command 'build-doc' to extract some documentation from the sources
  - Generate MySQL and SQLite schemas from UML models
  - Generate Ada database mappings from UML models
  - Generate Ada beans from the UML models
  - New project template for command line tools using ADO
  - New distribution command to merge the resource bundles

Version 0.5.0   - May 2012
  - Support multi-line comments in XML mappings
  - Generate List_Bean types for the XML mapped queries
  - Add support for Ada enum generation
  - Add test template generation
  - Add AWA service template generation
  - Add support for blob model mapping
  - New command 'add-ajax-form', 'add-query', 'dist', 'create-plugin'

Version 0.4.0	- Sep 2011
  - Generate ADO database queries
  - Generate Mysql and Sqlite schemas
  - Create the Mysql database and tables
  - Generate AWA modules with the new 'add-module' command

Version 0.3.0	- May 2011
  - Generate ADO database model
  - Generate a basic AWA/ASF project
