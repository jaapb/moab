CREATE DATABASE ocsipersist_moab;

CREATE EXTENSION citext;

CREATE SCHEMA moab
  CREATE TABLE users (
         userid varchar(16) primary key,
         firstname text NOT NULL,
         lastname text NOT NULL,
         email citext,
         password text,
         language text,
				 is_admin boolean NOT NULL DEFAULT(false)
  )

  CREATE TABLE activation (
         activationkey text primary key,
         userid varchar(16) NOT NULL references users(userid), -- DEFAULT
         email citext NOT NULL,
         autoconnect boolean NOT NULL,
         validity bigint NOT NULL,
         action text NOT NULL,
         data text NOT NULL,
         creationdate timestamptz NOT NULL default now()
  )
