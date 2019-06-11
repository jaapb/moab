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
		userid bigint not null,
		academic_year varchar(8) not null,
		learning_week smallint not null,
		title text not null,
		text text not null,
		approved boolean,
		primary key (userid, academic_year, learning_week),
		foreign key (userid, academic_year) references students (userid, academic_year)
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

	CREATE TABLE optional_sessions (
		session_id bigint not null,
		learning_week smallint not null,
		primary key (session_id, learning_week),
		foreign key (session_id) references sessions(session_id)
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
		userid bigint not null,
		assigned boolean not null default false,
		primary key (academic_year, userid),
		foreign key (userid, academic_year) references students(userid, academic_year)
	)

	CREATE TABLE presentation_criteria (
		academic_year varchar(8) not null,
		id bigserial not null,
		criterion text not null,
		description text,
		primary key (id)
	)	

	CREATE TABLE presentation_scores (
		academic_year varchar(8) not null,
		scorer_id bigint not null,
		presenter_id bigint not null,
		criterion_id bigint not null references presentation_criteria(id),
		score smallint not null,
		comment text,
		primary key (academic_year, scorer_id, presenter_id, criterion_id),
		foreign key (scorer_id) references ocsigen_start.users(userid),
		foreign key (presenter_id, academic_year) references students(userid, academic_year)
	)
	
	CREATE TABLE presentation_admin_scores (
		academic_year varchar(8) not null,
		presenter_id bigint not null,
		topic text not null,
		duration smallint not null,
		provisional_grade text not null,
		final_grade numeric(3,1),
		comments text not null,
		late_penalty boolean not null default false,
		primary key (academic_year, presenter_id),
		foreign key (academic_year, presenter_id) references students(academic_year, userid)
	)

	CREATE TABLE report_scores (
		academic_year varchar(8) not null,
		student_id bigint not null,
		quality_feedback text not null,
		quality_grade smallint not null,
		independence_feedback text not null,
		independence_grade smallint not null,
		community_feedback text not null,
		community_grade smallint not null,
		primary key (academic_year, student_id),
		foreign key (academic_year, student_id) references students(academic_year, userid)
	);
