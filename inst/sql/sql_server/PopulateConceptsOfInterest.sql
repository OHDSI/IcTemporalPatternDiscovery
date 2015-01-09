/*****************
IC temporal pattern discovery
a Self controlled cohort design

Store concept_Ids that will be used when calculating the results 

Tomas Bergvall
******************/

{DEFAULT @analysisId = 1}
{DEFAULT @conceptType = 1}
{DEFAULT @conceptId = 1}

USE @resultsSchema;

IF OBJECT_ID('ConceptsOfInterest', 'U') IS NULL 
CREATE TABLE ConceptsOfInterest(
  analysisId  INT NOT NULL,
  type        INT NOT NULL,
  id          INT NOT NULL,
CONSTRAINT pk_ConceptsOfInterest PRIMARY KEY (type, id, analysisId)
)

DELETE FROM   
ConceptsOfInterest 
WHERE analysisId = @analysisId
AND type =  @conceptType
AND id = @conceptId
;

INSERT INTO ConceptsOfInterest
VALUES(@analysisId, @conceptType, @conceptId)
