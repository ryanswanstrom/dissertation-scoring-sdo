-- Create the scoring tables

CREATE TABLE QUALITY_SCORE (
	QUALITY_SCORE_ID RAW(16) NOT NULL PRIMARY KEY,
	APPLICATION_ID VARCHAR2(64) NOT NULL,
	FREQ_DATE DATE NOT NULL,
	SCORE NUMBER(10,5) NOT NULL,
	ACTIVE CHAR DEFAULT 'Y' NOT NULL,
	INSERT_DATE DATE DEFAULT SYSDATE NOT NULL,
	UPDATE_DATE DATE DEFAULT SYSDATE NOT NULL
);

CREATE TABLE AVAILABILITY_SCORE (
	AVAILABILITY_SCORE_ID RAW(16) NOT NULL PRIMARY KEY,
	SERVICE_ID VARCHAR2(64) NOT NULL,
	FREQ_DATE DATE NOT NULL,
	SCORE NUMBER(10,5) NOT NULL,
	ACTIVE CHAR DEFAULT 'Y' NOT NULL,
	INSERT_DATE DATE DEFAULT SYSDATE NOT NULL,
	UPDATE_DATE DATE DEFAULT SYSDATE NOT NULL
);

CREATE TABLE SATISFACTION_SCORE (
	AVAILABILITY_SCORE_ID RAW(16) NOT NULL PRIMARY KEY,
	QUESTION_ID VARCHAR2(64) NOT NULL,
	FREQ_DATE DATE NOT NULL,
	SCORE NUMBER(10,5) NOT NULL,
	ACTIVE CHAR DEFAULT 'Y' NOT NULL,
	INSERT_DATE DATE DEFAULT SYSDATE NOT NULL,
	UPDATE_DATE DATE DEFAULT SYSDATE NOT NULL
);

CREATE TABLE SCHEDULE_SCORE (
	QUALITY_SCORE_ID RAW(16) NOT NULL PRIMARY KEY,
	PROJECT_ID VARCHAR2(64) NOT NULL,
	FREQ_DATE DATE NOT NULL,
	SCORE NUMBER(10,5) NOT NULL,
	ACTIVE CHAR DEFAULT 'Y' NOT NULL,
	INSERT_DATE DATE DEFAULT SYSDATE NOT NULL,
	UPDATE_DATE DATE DEFAULT SYSDATE NOT NULL
);

CREATE TABLE REQUIREMENTS_SCORE (
	REQUIREMENTS_SCORE_ID RAW(16) NOT NULL PRIMARY KEY,
	PROJECT_ID VARCHAR2(64) NOT NULL,
	FREQ_DATE DATE NOT NULL,
	SCORE NUMBER(10,5) NOT NULL,
	ACTIVE CHAR DEFAULT 'Y' NOT NULL,
	INSERT_DATE DATE DEFAULT SYSDATE NOT NULL,
	UPDATE_DATE DATE DEFAULT SYSDATE NOT NULL
);

-- remove the scoring tables
--DROP TABLE QUALITY_SCORE;
--DROP TABLE AVAILABILITY_SCORE;
--DROP TABLE SATISFACTION_SCORE;
--DROP TABLE SCHEDULE_SCORE;
--DROP TABLE REQUIREMENTS_SCORE;
