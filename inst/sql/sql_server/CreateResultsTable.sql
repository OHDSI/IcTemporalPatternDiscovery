/*****************
IC temporal pattern discovery
a Self controlled cohort design

Store analysis details and results

Tomas Bergvall
******************/

{DEFAULT @analysisId = 1}
{DEFAULT @sourceName = ""}
{DEFAULT @resultsSchema = ""}
{DEFAULT @resultsTablePrefix = "ictpd"}

USE @resultsSchema;

IF OBJECT_ID('@resultsTablePrefix_results', 'U') IS NOT NULL 
  DROP TABLE @resultsTablePrefix_results;


CREATE TABLE @resultsTablePrefix_results (
analysisId          INT NOT NULL,
sourceName          varchar(255) NOT NULL,
exposureConceptId   INT NOT NULL,
outcomeConceptId    INT NOT NULL,
CXY                 INT,
CX                  INT,
CY                  INT,
C                   INT,
expected            FLOAT,
ic                  FLOAT,
icLow               FLOAT,
icHigh              FLOAT,
CONSTRAINT pk_@resultsTablePrefix_results PRIMARY KEY (analysisId, sourceName, exposureConceptId, outcomeConceptId)
);



