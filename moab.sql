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
	);

CREATE SCHEMA moab
	CREATE TABLE term_sessions (
		term varchar(8) not null,
		session_id bigserial not null,
		year smallint not null,
		start_week smallint not null,
		end_week smallint not null,
		primary key (term, session_id)
	)

	CREATE TABLE students (
		userid bigint primary key references ocsigen_start.users(userid),
		student_id varchar(9) not null
	);

	CREATE TABLE blogs (
		userid bigint not null references ocsigen_start.users(userid),
		term varchar(8) not null,
		week smallint not null,
		title text not null,
		text text not null,
		primary key (userid, term, week)
	);
