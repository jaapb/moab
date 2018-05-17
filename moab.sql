--
-- PostgreSQL database dump
--

-- Dumped from database version 10.3
-- Dumped by pg_dump version 10.3

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'SQL_ASCII';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
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


--
-- Name: pgcrypto; Type: EXTENSION; Schema: -; Owner: -
--

CREATE EXTENSION IF NOT EXISTS pgcrypto WITH SCHEMA public;


--
-- Name: EXTENSION pgcrypto; Type: COMMENT; Schema: -; Owner: -
--

COMMENT ON EXTENSION pgcrypto IS 'cryptographic functions';


SET default_tablespace = '';

SET default_with_oids = false;

--
-- Name: attendance; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.attendance (
    session_id integer NOT NULL,
    user_id character varying(16) NOT NULL,
    learning_week smallint NOT NULL,
    confirmed character(1)
);


--
-- Name: blogs; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.blogs (
    user_id character varying(16) NOT NULL,
    learning_week smallint NOT NULL,
    term smallint NOT NULL,
    title text NOT NULL,
    contents text NOT NULL,
    approved boolean DEFAULT false NOT NULL
);


--
-- Name: eliom__persistent_refs; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.eliom__persistent_refs (
    key text NOT NULL,
    value bytea
);


--
-- Name: log; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.log (
    "time" timestamp without time zone NOT NULL,
    user_id character varying(8) NOT NULL,
    ip_address text NOT NULL,
    action character(1) NOT NULL
);


--
-- Name: optional_sessions; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.optional_sessions (
    timetable_id smallint NOT NULL,
    week smallint NOT NULL
);


--
-- Name: presentation_criteria; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.presentation_criteria (
    term smallint NOT NULL,
    id integer NOT NULL,
    criterion text NOT NULL,
    description text
);


--
-- Name: presentation_criteria_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.presentation_criteria_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: presentation_criteria_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.presentation_criteria_id_seq OWNED BY public.presentation_criteria.id;


--
-- Name: presentation_scores; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.presentation_scores (
    presenter_id character varying(16) NOT NULL,
    scorer_id character varying(16) NOT NULL,
    term smallint NOT NULL,
    criterion_id integer NOT NULL,
    score smallint NOT NULL,
    comment text NOT NULL
);


--
-- Name: presentation_tutor_feedback; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.presentation_tutor_feedback (
    presenter_id character varying(16) NOT NULL,
    scorer_id character varying(16) NOT NULL,
    term smallint NOT NULL,
    comments text NOT NULL,
    duration smallint NOT NULL,
    topic text NOT NULL,
    provisional_grade character varying(16) NOT NULL,
    final_grade double precision
);


--
-- Name: report_scores; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.report_scores (
    user_id character varying(16) NOT NULL,
    term smallint NOT NULL,
    quality_score integer NOT NULL,
    quality_feedback text NOT NULL,
    independence_score integer NOT NULL,
    independence_feedback text NOT NULL,
    communication_score integer NOT NULL,
    communication_feedback text NOT NULL
);


--
-- Name: schedule; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.schedule (
    user_id character varying(16) NOT NULL,
    first boolean NOT NULL,
    learning_week smallint NOT NULL,
    timetable_id integer NOT NULL,
    set character(1) NOT NULL
);


--
-- Name: sessions; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.sessions (
    year smallint NOT NULL,
    start_week smallint NOT NULL,
    end_week smallint NOT NULL,
    id integer NOT NULL,
    timetable_id integer NOT NULL
);


--
-- Name: sessions_session_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.sessions_session_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: sessions_session_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.sessions_session_id_seq OWNED BY public.sessions.id;


--
-- Name: students; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.students (
    user_id character varying(16) NOT NULL,
    group_number smallint NOT NULL,
    student_id character(9) NOT NULL,
    joined_week smallint NOT NULL,
    left_week smallint,
    visa boolean DEFAULT false NOT NULL,
    term smallint NOT NULL
);


--
-- Name: timetable; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.timetable (
    weekday smallint NOT NULL,
    start_time time without time zone NOT NULL,
    end_time time without time zone NOT NULL,
    term smallint NOT NULL,
    id integer NOT NULL,
    type character(1) NOT NULL,
    group_number smallint,
    locked boolean,
    CONSTRAINT timetable_check CHECK ((((type = 'S'::bpchar) AND (group_number IS NOT NULL) AND (locked IS NOT NULL)) OR ((group_number IS NULL) AND (locked IS NULL))))
);


--
-- Name: timetable_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE public.timetable_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: timetable_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE public.timetable_id_seq OWNED BY public.timetable.id;


--
-- Name: users; Type: TABLE; Schema: public; Owner: -
--

CREATE TABLE public.users (
    id character varying(16) NOT NULL,
    is_admin boolean DEFAULT false NOT NULL,
    password text NOT NULL,
    first_name text NOT NULL,
    last_name text NOT NULL
);


--
-- Name: presentation_criteria id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.presentation_criteria ALTER COLUMN id SET DEFAULT nextval('public.presentation_criteria_id_seq'::regclass);


--
-- Name: sessions id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.sessions ALTER COLUMN id SET DEFAULT nextval('public.sessions_session_id_seq'::regclass);


--
-- Name: timetable id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.timetable ALTER COLUMN id SET DEFAULT nextval('public.timetable_id_seq'::regclass);


--
-- Name: attendance attendance_session_id_user_id_week_key; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.attendance
    ADD CONSTRAINT attendance_session_id_user_id_week_key UNIQUE (session_id, user_id, learning_week);


--
-- Name: blogs blogs_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.blogs
    ADD CONSTRAINT blogs_pkey PRIMARY KEY (user_id, learning_week, term);


--
-- Name: eliom__persistent_refs eliom__persistent_refs_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.eliom__persistent_refs
    ADD CONSTRAINT eliom__persistent_refs_pkey PRIMARY KEY (key);


--
-- Name: optional_sessions optional_sessions_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.optional_sessions
    ADD CONSTRAINT optional_sessions_pkey PRIMARY KEY (timetable_id, week);


--
-- Name: presentation_criteria presentation_criteria_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.presentation_criteria
    ADD CONSTRAINT presentation_criteria_pkey PRIMARY KEY (term, id);


--
-- Name: presentation_scores presentation_scores_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.presentation_scores
    ADD CONSTRAINT presentation_scores_pkey PRIMARY KEY (presenter_id, scorer_id, term, criterion_id);


--
-- Name: presentation_tutor_feedback presentation_tutor_feedback_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.presentation_tutor_feedback
    ADD CONSTRAINT presentation_tutor_feedback_pkey PRIMARY KEY (presenter_id, scorer_id, term);


--
-- Name: report_scores report_scores_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.report_scores
    ADD CONSTRAINT report_scores_pkey PRIMARY KEY (user_id, term);


--
-- Name: schedule schedule_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.schedule
    ADD CONSTRAINT schedule_pkey PRIMARY KEY (user_id, timetable_id);


--
-- Name: sessions sessions_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.sessions
    ADD CONSTRAINT sessions_pkey PRIMARY KEY (id);


--
-- Name: students students_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.students
    ADD CONSTRAINT students_pkey PRIMARY KEY (user_id, term);


--
-- Name: timetable timetable_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.timetable
    ADD CONSTRAINT timetable_pkey PRIMARY KEY (id);


--
-- Name: users users_pkey; Type: CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.users
    ADD CONSTRAINT users_pkey PRIMARY KEY (id);


--
-- Name: attendance attendance_session_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.attendance
    ADD CONSTRAINT attendance_session_id_fkey FOREIGN KEY (session_id) REFERENCES public.sessions(id);


--
-- Name: attendance attendance_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.attendance
    ADD CONSTRAINT attendance_user_id_fkey FOREIGN KEY (user_id) REFERENCES public.users(id);


--
-- Name: optional_sessions optional_sessions_timetable_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.optional_sessions
    ADD CONSTRAINT optional_sessions_timetable_id_fkey FOREIGN KEY (timetable_id) REFERENCES public.timetable(id);


--
-- Name: presentation_scores presentation_scores_presenter_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.presentation_scores
    ADD CONSTRAINT presentation_scores_presenter_id_fkey FOREIGN KEY (presenter_id) REFERENCES public.users(id);


--
-- Name: presentation_scores presentation_scores_presenter_id_fkey1; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.presentation_scores
    ADD CONSTRAINT presentation_scores_presenter_id_fkey1 FOREIGN KEY (presenter_id, term) REFERENCES public.students(user_id, term);


--
-- Name: presentation_scores presentation_scores_scorer_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.presentation_scores
    ADD CONSTRAINT presentation_scores_scorer_id_fkey FOREIGN KEY (scorer_id) REFERENCES public.users(id);


--
-- Name: presentation_scores presentation_scores_term_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.presentation_scores
    ADD CONSTRAINT presentation_scores_term_fkey FOREIGN KEY (term, criterion_id) REFERENCES public.presentation_criteria(term, id);


--
-- Name: presentation_tutor_feedback presentation_tutor_feedback_presenter_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.presentation_tutor_feedback
    ADD CONSTRAINT presentation_tutor_feedback_presenter_id_fkey FOREIGN KEY (presenter_id, term) REFERENCES public.students(user_id, term);


--
-- Name: presentation_tutor_feedback presentation_tutor_feedback_scorer_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.presentation_tutor_feedback
    ADD CONSTRAINT presentation_tutor_feedback_scorer_id_fkey FOREIGN KEY (scorer_id) REFERENCES public.users(id);


--
-- Name: report_scores report_scores_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.report_scores
    ADD CONSTRAINT report_scores_user_id_fkey FOREIGN KEY (user_id, term) REFERENCES public.students(user_id, term);


--
-- Name: schedule schedule_timetable_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.schedule
    ADD CONSTRAINT schedule_timetable_id_fkey FOREIGN KEY (timetable_id) REFERENCES public.timetable(id);


--
-- Name: sessions sessions_timetable_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.sessions
    ADD CONSTRAINT sessions_timetable_id_fkey FOREIGN KEY (timetable_id) REFERENCES public.timetable(id);


--
-- Name: students students_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY public.students
    ADD CONSTRAINT students_user_id_fkey FOREIGN KEY (user_id) REFERENCES public.users(id);


--
-- PostgreSQL database dump complete
--

