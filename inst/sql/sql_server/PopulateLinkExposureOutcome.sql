/*****************
IC temporal pattern discovery
a Self controlled cohort design

Store concept_Ids that will be used when calculating the results 

Tomas Bergvall
******************/

{DEFAULT @analysisId = 1}
{DEFAULT @exposureConceptId = 1}
{DEFAULT @outcomeConceptId = 1}

USE @resultsSchema;

IF OBJECT_ID('LinkExposureConcept', 'U') IS NULL 
CREATE TABLE LinkExposureConcept(
  analysisId        INT NOT NULL,
  exposureConceptId INT NOT NULL,
  outcomeConceptId  INT NOT NULL,
CONSTRAINT pk_LinkExposureConcept PRIMARY KEY (exposureConceptId, outcomeConceptId, analysisId)
)

DELETE FROM   
LinkExposureConcept 
WHERE analysisId = @analysisId
AND exposureConceptId =  @exposureConceptId
AND outcomeConceptId = @outcomeConceptId
;

INSERT INTO LinkExposureConcept
VALUES(@analysisId, @exposureConceptId, @outcomeConceptId)
