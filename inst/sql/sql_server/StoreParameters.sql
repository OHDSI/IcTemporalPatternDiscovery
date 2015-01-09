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
{DEFAULT @exposureTable = 'drug_era'} --name of table where contains in format of DRUG_ERA live (could be temp table if pre-processing was necessary)
{DEFAULT @exposureStartDate = 'drug_era_start_date'} 
{DEFAULT @exposureEndDate = 'drug_era_end_date'} 
{DEFAULT @exposureConceptId = 'drug_concept_id'} 
{DEFAULT @outcomeTable = 'condition_era'} --name of table where contains in format of CONDITION_ERA live (could be temp table if pre-processing was necessary)
{DEFAULT @outcomeStartDate = 'condition_era_start_date'} 
{DEFAULT @outcomeEndDate = 'condition_era_end_date'} 
{DEFAULT @outcomeConceptId = 'condition_concept_id'} 
{DEFAULT @personTable = 'person'} --name of table where contains in format of PERSON live (could be temp table if pre-processing was necessary)
{DEFAULT @observationPeriodTable = 'observation_period'} --name of table where contains in format of OBSERVATION_PERIOD live (could be temp table if pre-processing was necessary)

{DEFAULT @drugTypeConceptIdList = '38000182'} --which DRUG_TYPE to use:  generally only use 1 value (ex:  30d era)
{DEFAULT @conditionTypeConceptIdList = '38000247'} --which CONDITION_TYPE to use:  generally only use 1 value (ex:  30d era)

{DEFAULT @listExposureConceptId = "1,2,3"} --which drug_concept_ids to use
{DEFAULT @listOutcomeConceptId = "1,2,3"} --which condition_concept_ids to use

{DEFAULT @controlPeriodStart = "-1080"}
{DEFAULT @controlPeriodEnd = "-361"}
{DEFAULT @customObservationPeriodStart = "1"}
{DEFAULT @customObservationPeriodEnd = "30"}
{DEFAULT @censor = "0"}  
{DEFAULT @countEntity = "drug_era_id"}

{DEFAULT @multipeControlPeriods = "110"}
{DEFAULT @multipeObservationPeriods = "10000"}
{DEFAULT @shrinkage = 0.50}
{DEFAULT @icPercentile = 0.025}
{DEFAULT @metric = "IC025"}
                                     



USE @resultsSchema;

IF OBJECT_ID('@resultsTablePrefix_analysis', 'U') IS NOT NULL 
  DROP TABLE @resultsTablePrefix_analysis;

CREATE TABLE @resultsTablePrefix_analysis (
  analysisId INT NOT NULL,
  parameterName varchar(255) NOT NULL,
  value varchar(max),
  CONSTRAINT pk_@resultsTablePrefix_analysis PRIMARY KEY (analysisId, parameterName)
);

DELETE FROM 
  @resultsTablePrefix_analysis 
WHERE analysisId = @analysisId;

INSERT INTO
	@resultsTablePrefix_analysis
      SELECT @analysisId, 'exposureTable', '@exposureTable'
UNION SELECT @analysisId, 'exposureStartDate', '@exposureStartDate'
UNION SELECT @analysisId, 'exposureEndDate', '@exposureEndDate'
UNION SELECT @analysisId, 'exposureConceptId', '@exposureConceptId'
UNION SELECT @analysisId, 'outcomeTable', '@outcomeTable'
UNION SELECT @analysisId, 'outcomeStartDate', '@outcomeStartDate'
UNION SELECT @analysisId, 'outcomeEndDate', '@outcomeEndDate'
UNION SELECT @analysisId, 'outcomeConceptId', '@outcomeConceptId'
UNION SELECT @analysisId, 'personTable', '@personTable'
UNION SELECT @analysisId, 'observationPeriodTable', '@observationPeriodTable'
UNION SELECT @analysisId, 'drugTypeConceptIdList', '@drugTypeConceptIdList'
UNION SELECT @analysisId, 'conditionTypeConceptIdList', '@conditionTypeConceptIdList'
UNION SELECT @analysisId, 'controlPeriodStart', '@controlPeriodStart'
UNION SELECT @analysisId, 'controlPeriodEnd', '@controlPeriodEnd'
UNION SELECT @analysisId, 'censor', '@censor'
UNION SELECT @analysisId, 'countEntity', '@countEntity'
UNION SELECT @analysisId, 'multipeControlPeriods', '@multipeControlPeriods'
UNION SELECT @analysisId, 'multipeObservationPeriods', '@multipeObservationPeriods'
UNION SELECT @analysisId, 'shrinkage', '@shrinkage'
UNION SELECT @analysisId, 'icPercentile', '@icPercentile'
UNION SELECT @analysisId, 'metric', '@metric'
UNION SELECT @analysisId, 'exposureConceptIds', '@listExposureConceptId'
UNION SELECT @analysisId, 'outcomeConceptIds', '@listOutcomeConceptId'
UNION SELECT @analysisId, 'metric', '@metric'
UNION SELECT @analysisId, 'customObservationPeriodStart', '@customObservationPeriodStart'
UNION SELECT @analysisId, 'customObservationPeriodEnd', '@customObservationPeriodEnd'
;
