--
-- PostgreSQL database dump
--

-- Dumped from database version 9.6.5
-- Dumped by pg_dump version 9.6.5

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'SQL_ASCII';
SET standard_conforming_strings = on;
SET check_function_bodies = false;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: plpgsql; Type: EXTENSION; Schema: -; Owner: -
--

CREATE EXTENSION IF NOT EXISTS plpgsql WITH SCHEMA pg_catalog;


--
-- Name: EXTENSION plpgsql; Type: COMMENT; Schema: -; Owner: -
--

COMMENT ON EXTENSION plpgsql IS 'PL/pgSQL procedural language';


SET search_path = public, pg_catalog;

SET default_tablespace = '';

SET default_with_oids = false;

--
-- Name: attendance; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE attendance (
    session_id integer NOT NULL,
    user_id character varying(16) NOT NULL,
    week smallint NOT NULL
);


--
-- Name: log; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE log (
    "time" timestamp without time zone NOT NULL,
    user_id character varying(8) NOT NULL,
    ip_address text NOT NULL,
    action character(1) NOT NULL
);


--
-- Name: schedule; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE schedule (
    user_id character varying(16) NOT NULL,
    session_id integer NOT NULL,
    first boolean NOT NULL,
    week smallint NOT NULL
);


--
-- Name: sessions; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE sessions (
    year smallint NOT NULL,
    start_week smallint NOT NULL,
    end_week smallint NOT NULL,
    id integer NOT NULL,
    timetable_id integer NOT NULL
);


--
-- Name: sessions_session_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE sessions_session_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: sessions_session_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE sessions_session_id_seq OWNED BY sessions.id;


--
-- Name: timetable; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE timetable (
    weekday smallint NOT NULL,
    start_time time without time zone NOT NULL,
    end_time time without time zone NOT NULL,
    term smallint NOT NULL,
    id integer NOT NULL,
    type character(1) NOT NULL,
    group_number smallint
);


--
-- Name: timetable_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE timetable_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: timetable_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE timetable_id_seq OWNED BY timetable.id;


--
-- Name: users; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE users (
    id character varying(16) NOT NULL,
    is_admin boolean DEFAULT false NOT NULL,
    name text NOT NULL,
    group_number smallint
);


--
-- Name: sessions id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY sessions ALTER COLUMN id SET DEFAULT nextval('sessions_session_id_seq'::regclass);


--
-- Name: timetable id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY timetable ALTER COLUMN id SET DEFAULT nextval('timetable_id_seq'::regclass);


--
-- Name: attendance attendance_session_id_user_id_week_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY attendance
    ADD CONSTRAINT attendance_session_id_user_id_week_key UNIQUE (session_id, user_id, week);


--
-- Name: sessions sessions_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY sessions
    ADD CONSTRAINT sessions_pkey PRIMARY KEY (id);


--
-- Name: timetable timetable_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY timetable
    ADD CONSTRAINT timetable_pkey PRIMARY KEY (id);


--
-- Name: users users_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY users
    ADD CONSTRAINT users_pkey PRIMARY KEY (id);


--
-- Name: attendance attendance_session_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY attendance
    ADD CONSTRAINT attendance_session_id_fkey FOREIGN KEY (session_id) REFERENCES sessions(id);


--
-- Name: attendance attendance_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY attendance
    ADD CONSTRAINT attendance_user_id_fkey FOREIGN KEY (user_id) REFERENCES users(id);


--
-- Name: log log_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY log
    ADD CONSTRAINT log_user_id_fkey FOREIGN KEY (user_id) REFERENCES users(id);


--
-- Name: schedule schedule_session_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY schedule
    ADD CONSTRAINT schedule_session_id_fkey FOREIGN KEY (session_id) REFERENCES sessions(id);


--
-- Name: schedule schedule_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY schedule
    ADD CONSTRAINT schedule_user_id_fkey FOREIGN KEY (user_id) REFERENCES users(id);


--
-- Name: sessions sessions_timetable_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY sessions
    ADD CONSTRAINT sessions_timetable_id_fkey FOREIGN KEY (timetable_id) REFERENCES timetable(id);


--
-- PostgreSQL database dump complete
--

