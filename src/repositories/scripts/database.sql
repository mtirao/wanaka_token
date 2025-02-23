--
-- PostgreSQL database dump
--

-- Dumped from database version 15.3
-- Dumped by pg_dump version 16.0

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

SET default_tablespace = '';

SET default_table_access_method = heap;

--
-- Name: realms; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.realms (
    client_id text NOT NULL,
    client_secret text NOT NULL,
    grant_type text NOT NULL
);


ALTER TABLE public.realms OWNER TO postgres;

--
-- Name: tokens; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.tokens (
    auth_token text NOT NULL,
    client_id text NOT NULL
);


ALTER TABLE public.tokens OWNER TO postgres;

--
-- Data for Name: realms; Type: TABLE DATA; Schema: public; Owner: postgres
--



--
-- Data for Name: tokens; Type: TABLE DATA; Schema: public; Owner: postgres
--



--
-- Name: realms realms_client_secret_key; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.realms
    ADD CONSTRAINT realms_client_secret_key UNIQUE (client_secret);


--
-- Name: tokens unique_tokens_token; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.tokens
    ADD CONSTRAINT unique_tokens_token UNIQUE (auth_token);


--
-- Name: realms_client_secret_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX realms_client_secret_idx ON public.realms USING btree (client_secret);


--
-- PostgreSQL database dump complete
--

