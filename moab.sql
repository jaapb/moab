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
		user_type char(1) NOT NULL DEFAULT('S')
  )

	CREATE TABLE emails (
		email citext primary key,
		userid bigint NOT NULL references users(userid),
		validated boolean NOT NULL DEFAULT(false)
	)

	CREATE TABLE activation (
		activationkey text primary key,
		userid bigint NOT NULL references users(userid),
		email citext NOT NULL,
		autoconnect boolean NOT NULL,
		validity bigint NOT NULL,
		action text NOT NULL,
		data text NOT NULL,
		creationdate timestamptz NOT NULL default now()
	);

CREATE SCHEMA moab
	CREATE TABLE terms (
		academic_year varchar(8) not null,
		term_id bigserial not null,
		year smallint not null,
		start_week smallint not null,
		end_week smallint not null,
		primary key (academic_year, term_id)
	)

	CREATE TABLE students (
		userid bigint references ocsigen_start.users(userid),
		academic_year varchar(8) not null,
		student_id varchar(9) not null,
		joined_week smallint not null,
    left_week smallint,
		group_number smallint,
		primary key (userid, academic_year),
		unique (student_id, academic_year)
	)

	CREATE TABLE blogs (
		userid bigint not null references ocsigen_start.users(userid),
		academic_year varchar(8) not null,
		learning_week smallint not null,
		title text not null,
		text text not null,
		approved boolean,
		primary key (userid, academic_year, week)
	)

	CREATE TABLE sessions (
		academic_year varchar(8) not null,
		term_id bigint not null,
		session_id bigserial not null primary key,
		session_type char(1) not null,
		start_time time not null,
		end_time time not null,	
		room varchar(8),
		weekday smallint not null,
		group_number smallint,
		foreign key (academic_year, term_id) references terms(academic_year, term_id) 
	)

	CREATE TABLE attendance (
		session_id bigint not null references sessions(session_id),
		userid bigint not null references ocsigen_start.users(userid),
		learning_week smallint not null,
		primary key (session_id, userid, learning_week)
	)

	CREATE TABLE presentation_schedule (
		academic_year varchar(8) not null,
		group_number smallint not null,
		learning_week smallint not null,
		first_presenter boolean not null default true,
		userid bigint not null references ocsigen_start.users(userid),
		primary key (academic_year, userid)
	);
