CREATE DATABASE ocsipersist_moab;

CREATE EXTENSION citext;
CREATE EXTENSION pgcrypto;

CREATE SCHEMA ocsigen_start
  CREATE TABLE users (
		userid bigserial primary key,
		firstname text NOT NULL,
		lastname text NOT NULL,
		main_email citext,
		password text,
		avatar text,
		language text,
		usertype char(1) NOT NULL DEFAULT('S')
  )

	CREATE TABLE emails (
		email citext primary key,
		userid bigint NOT NULL references users(userid),
		validated boolean NOT NULL DEFAULT(false)
	)
