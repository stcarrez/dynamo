/* File generated automatically by dynamo */
/* Application Schema information */
create table ADO_SCHEMA (
  /* the model name */
  `NAME` VARCHAR(256) UNIQUE NOT NULL,
  /* the schema version */
  `VERSION` INTEGER NOT NULL,
  /* the upgrade date */
  `DATE` DATETIME NOT NULL,
  primary key (`NAME`)
);
insert into entity_type (name) values
("ADO_SCHEMA")
;
