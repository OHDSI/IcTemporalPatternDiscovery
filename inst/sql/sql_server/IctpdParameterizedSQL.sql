/*****************
IC temporal pattern discovery
a Self controlled cohort design

Tomas Bergvall
******************/
{DEFAULT @analysisId = 1}
{DEFAULT @sourceName = ""}
{DEFAULT @cdmSchema = ""}
{DEFAULT @resultsSchema = ""}
{DEFAULT @exposureTable = "drug_era""} --name of table where contains in format of DRUG_ERA live (could be temp table if pre-processing was necessary)
{DEFAULT @exposureStartDate = 'drug_era_start_date'} 
{DEFAULT @exposureEndDate = 'drug_era_end_date'} 
{DEFAULT @exposureConceptId = 'drug_concept_id'} 
{DEFAULT @exposurePersonId = 'person_id'} 
{DEFAULT @outcomeTable = 'condition_era'} --name of table where contains in format of CONDITION_ERA live (could be temp table if pre-processing was necessary)
{DEFAULT @outcomeStartDate = 'condition_era_start_date'} 
{DEFAULT @outcomeEndDate = 'condition_era_end_date'} 
{DEFAULT @outcomeConceptId = 'condition_concept_id'} 
{DEFAULT @outcomePersonId = 'person_id'} 
{DEFAULT @personTable = 'person'} --name of table where contains in format of PERSON live (could be temp table if pre-processing was necessary)
{DEFAULT @observationPeriodTable = 'observation_period'} --name of table where contains in format of OBSERVATION_PERIOD live (could be temp table if pre-processing was necessary)

{DEFAULT @drugTypeConceptIdList = '38000182'} --which DRUG_TYPE to use:  generally only use 1 value (ex:  30d era)
{DEFAULT @conditionTypeConceptIdList = '38000247'} --which CONDITION_TYPE to use:  generally only use 1 value (ex:  30d era)

{DEFAULT @controlPeriodStart = -1080}
{DEFAULT @controlPeriodEnd = -361}
{DEFAULT @customObservationPeriodStart = 1,
{DEFAULT @customObservationPeriodEnd = 30, 
{DEFAULT @censor = '0'}  
{DEFAULT @countEntity = 'drug_era_id'}

USE @cdmSchema;

/*****************
PERSON_OPT
Should be persisted between runs
******************/
IF OBJECT_ID('PERSON_OPT', 'U') IS NULL 
CREATE TABLE PERSON_OPT(
  person_id     INT NOT NULL PRIMARY KEY,
  gender        INT,
  year_of_birth INT,
  reg_date      DATE,
  dereg_date    DATE
)

ELSE TRUNCATE TABLE PERSON_OPT

INSERT INTO PERSON_OPT
SELECT
  PE.person_id                     as person_id,
  PE.gender_concept_id             as gender,
  PE.year_of_birth                 as year_of_birth,
  CASE 
    WHEN MIN(OP.observation_period_start_date) < MIN(DE.@exposureStartDate)
    THEN MIN(OP.observation_period_start_date)
    ELSE MIN(DE.@exposureStartDate)
  END as reg_date,
  CASE 
    WHEN MAX(OP.observation_period_end_date) > MAX(DE.@exposureEndDate)
    THEN MAX(OP.observation_period_end_date)
    ELSE MAX(DE.@exposureEndDate)
  END as dereg_date
FROM @personTable PE
INNER JOIN @observationPeriodTable OP on (PE.person_id = OP.person_id)
INNER JOIN @exposureTable      DE on (PE.person_id = DE.person_id)
WHERE DE.drug_type_concept_id in (@drugTypeConceptIdList)
  --AND mod( PE.person_id, 1000 ) = 0
GROUP BY PE.person_id, PE.gender_concept_id, PE.year_of_birth

/*****************
BASE_LINE_COUNTER_C
Can be persisted between runs IF standard comparartor group is used
******************/
IF OBJECT_ID('BASE_LINE_COUNTER_C', 'U') IS NULL 
CREATE TABLE BASE_LINE_COUNTER_C (
  AnalysisId           INT NOT NULL,
  c_control            INT,
  c_1m                  INT,
  c_0m            	   INT,
  c_observed_1_30 	   INT,
  c_observed_1_360 	   INT,
  c_observed_31_90   	 INT,
  c_observed_91_180  	 INT,
  c_observed_721_1080	 INT,
  CONSTRAINT pk_BASE_LINE_COUNTER_C PRIMARY KEY (AnalysisId)
) 
ELSE truncate table BASE_LINE_COUNTER_C


INSERT INTO BASE_LINE_COUNTER_C
SELECT @analysisId,
  COUNT(DISTINCT CASE WHEN DATEDIFF(dd, DE.@exposureStartDate, PA.reg_date) <= @controlPeriodEnd THEN CAST(DE.@countEntity as varchar) + '_' + CAST(DE.@exposureConceptId as varchar) ELSE NULL END) as c_control,
  COUNT(DISTINCT CASE WHEN DATEDIFF(dd, DE.@exposureStartDate, PA.reg_date) <= -1                THEN CAST(DE.@countEntity as varchar) + '_' + CAST(DE.@exposureConceptId as varchar) ELSE NULL END) as c_1m,
  COUNT(DISTINCT CASE WHEN DATEDIFF(dd, DE.@exposureStartDate, PA.reg_date) <= 0                 THEN CAST(DE.@countEntity as varchar) + '_' + CAST(DE.@exposureConceptId as varchar) ELSE NULL END) as c_0m,
  COUNT(DISTINCT CASE WHEN DATEDIFF(dd, DE.@exposureStartDate, {@censor == 1} ? {LEAST(PA.dereg_date, DE.@exposureEndDate)} : {PA.dereg_date}) >= @customObservationPeriodStart           THEN CAST(DE.@countEntity as varchar) + '_' + CAST(DE.@exposureConceptId as varchar) ELSE NULL END) as c_observed_1_30,
  COUNT(DISTINCT CASE WHEN DATEDIFF(dd, DE.@exposureStartDate, {@censor == 1} ? {LEAST(PA.dereg_date, DE.@exposureEndDate)} : {PA.dereg_date}) >= 1           THEN CAST(DE.@countEntity as varchar) + '_' + CAST(DE.@exposureConceptId as varchar) ELSE NULL END) as c_observed_1_360,
  COUNT(DISTINCT CASE WHEN DATEDIFF(dd, DE.@exposureStartDate, {@censor == 1} ? {LEAST(PA.dereg_date, DE.@exposureEndDate)} : {PA.dereg_date}) >= 31          THEN CAST(DE.@countEntity as varchar) + '_' + CAST(DE.@exposureConceptId as varchar) ELSE NULL END) as c_observed_31_90,
  COUNT(DISTINCT CASE WHEN DATEDIFF(dd, DE.@exposureStartDate, {@censor == 1} ? {LEAST(PA.dereg_date, DE.@exposureEndDate)} : {PA.dereg_date}) >= 91          THEN CAST(DE.@countEntity as varchar) + '_' + CAST(DE.@exposureConceptId as varchar) ELSE NULL END) as c_observed_91_180,
  COUNT(DISTINCT CASE WHEN DATEDIFF(dd, DE.@exposureStartDate, {@censor == 1} ? {LEAST(PA.dereg_date, DE.@exposureEndDate)} : {PA.dereg_date}) >= 721         THEN CAST(DE.@countEntity as varchar) + '_' + CAST(DE.@exposureConceptId as varchar) ELSE NULL END) as c_observed_721_1080
FROM @exposureTable DE
JOIN PERSON_OPT              PA ON (DE.person_id= PA.person_id)
WHERE DE.drug_type_concept_id in (@drugTypeConceptIdList)



/*****************
BASE_LINE_COUNTER_CY
Can be persisted between runs IF standard comparartor group is used
******************/
IF OBJECT_ID('BASE_LINE_COUNTER_CY', 'U') IS NULL 
CREATE TABLE BASE_LINE_COUNTER_CY (
  AnalysisId            INT NOT NULL,
  @outcomeConceptId     INT NOT NULL,
  cy_control            INT,
  cy_1m                 INT,
  cy_0m                  INT,
  cy_observed_1_30 	    INT,
  cy_observed_1_360 	  INT,
  cy_observed_31_90   	INT,
  cy_observed_91_180  	INT,
  cy_observed_721_1080	INT,
  CONSTRAINT pk_BASE_LINE_COUNTER_CY PRIMARY KEY (AnalysisId, @outcomeConceptId)
) 
ELSE truncate table BASE_LINE_COUNTER_CY 


INSERT INTO BASE_LINE_COUNTER_CY
SELECT @analysisId,  CE.@outcomeConceptId,
  COUNT(distinct CASE WHEN DATEDIFF(dd, DE.@exposureStartDate, CE.@outcomeStartDate) between @controlPeriodStart AND @controlPeriodEnd THEN CAST(DE.@countEntity as varchar) + '_' + CAST(DE.@exposureConceptId as varchar) ELSE NULL END) AS cy_control,
  COUNT(distinct CASE WHEN DATEDIFF(dd, DE.@exposureStartDate, CE.@outcomeStartDate) between -30 AND -1 THEN CAST(DE.@countEntity as varchar) + '_' + CAST(DE.@exposureConceptId as varchar) ELSE NULL END) AS cy_1m,
  COUNT(distinct CASE WHEN DATEDIFF(dd, DE.@exposureStartDate, CE.@outcomeStartDate) = 0 THEN CAST(DE.@countEntity as varchar) + '_' + CAST(DE.@exposureConceptId as varchar) ELSE NULL END) AS cy_0m,
  COUNT(distinct CASE WHEN DATEDIFF(dd, DE.@exposureStartDate, CE.@outcomeStartDate) between @customObservationPeriodStart AND @customObservationPeriodEnd {@censor == 1} ? {AND CE.@outcomeStartDate < DE.@exposureEndDate} : {} THEN CAST(DE.@countEntity as varchar) + '_' + CAST(DE.@exposureConceptId as varchar) ELSE NULL END) AS cy_observed_1_30,
  COUNT(distinct CASE WHEN DATEDIFF(dd, DE.@exposureStartDate, CE.@outcomeStartDate) between 1 AND 360 {@censor == 1} ? {AND CE.@outcomeStartDate < DE.@exposureEndDate} : {} THEN CAST(DE.@countEntity as varchar) + '_' + CAST(DE.@exposureConceptId as varchar) ELSE NULL END) AS cy_observed_1_360,
  COUNT(distinct CASE WHEN DATEDIFF(dd, DE.@exposureStartDate, CE.@outcomeStartDate) between 31 AND 90 {@censor == 1} ? {AND CE.@outcomeStartDate < DE.@exposureEndDate} : {} THEN CAST(DE.@countEntity as varchar) + '_' + CAST(DE.@exposureConceptId as varchar) ELSE NULL END) AS cy_observed_31_90,
  COUNT(distinct CASE WHEN DATEDIFF(dd, DE.@exposureStartDate, CE.@outcomeStartDate) between 91 AND 180 {@censor == 1} ? {AND CE.@outcomeStartDate < DE.@exposureEndDate} : {} THEN CAST(DE.@countEntity as varchar) + '_' + CAST(DE.@exposureConceptId as varchar) ELSE NULL END) AS cy_observed_91_180,
  COUNT(distinct CASE WHEN DATEDIFF(dd, DE.@exposureStartDate, CE.@outcomeStartDate) between 721 AND 1080 {@censor == 1} ? {AND CE.@outcomeStartDate < DE.@exposureEndDate} : {} THEN CAST(DE.@countEntity as varchar) + '_' + CAST(DE.@exposureConceptId as varchar) ELSE NULL END) AS cy_observed_721_1080
FROM @outcomeTable   CE
JOIN @exposureTable  DE ON (CE.person_id = DE.person_id)
JOIN ConceptsOfInterest                               COI ON (CE.@outcomeConceptId = COI.id AND COI.type = 2)
JOIN PERSON_OPT                                       PA  ON (DE.person_id= PA.person_id)

WHERE DE.drug_type_concept_id in (@drugTypeConceptIdList)
  AND CE.condition_type_concept_id in (@conditionTypeConceptIdList)
  AND CE.@outcomeStartDate between PA.reg_date and PA.dereg_date

GROUP BY CE.@outcomeConceptId

/*****************
BASE_LINE_COUNTER_CX
******************/
IF OBJECT_ID('BASE_LINE_COUNTER_CX', 'U') IS NULL 
CREATE TABLE BASE_LINE_COUNTER_CX(
    AnalysisId            INT NOT NULL,
    @exposureConceptId       INT NOT NULL,
    cx_control               INT,
    cx_1m                    INT,
    cx_0m                    INT,
    cx_observed_1_30         INT,
    cx_observed_1_360 	     INT,
    cx_observed_31_90   	   INT,
    cx_observed_91_180  	   INT,
    cx_observed_721_1080	   INT,
  CONSTRAINT pk_BASE_LINE_COUNTER_CX PRIMARY KEY (AnalysisId, @exposureConceptId)
  )
ELSE truncate table BASE_LINE_COUNTER_CX


INSERT INTO BASE_LINE_COUNTER_CX 
SELECT @analysisId, DE.@exposureConceptId, 
  COUNT(DISTINCT CASE WHEN DATEDIFF(dd, DE.@exposureStartDate, PA.reg_date) <= @controlPeriodEnd THEN CAST(DE.@countEntity as varchar) + '_' + CAST(DE.@exposureConceptId as varchar) ELSE NULL END) as cx_control,
  COUNT(DISTINCT CASE WHEN DATEDIFF(dd, DE.@exposureStartDate, PA.reg_date) <= -1                THEN CAST(DE.@countEntity as varchar) + '_' + CAST(DE.@exposureConceptId as varchar) ELSE NULL END) as cx_1m,
  COUNT(DISTINCT CASE WHEN DATEDIFF(dd, DE.@exposureStartDate, PA.reg_date) <= 0                 THEN CAST(DE.@countEntity as varchar) + '_' + CAST(DE.@exposureConceptId as varchar) ELSE NULL END) as cx_0m,
  COUNT(DISTINCT CASE WHEN DATEDIFF(dd, DE.@exposureStartDate, {@censor == 1} ? {LEAST(PA.dereg_date, DE.@exposureEndDate)} : {PA.dereg_date}) >= @customObservationPeriodStart THEN CAST(DE.@countEntity as varchar) + '_' + CAST(DE.@exposureConceptId as varchar) ELSE NULL END) as cx_observed_1_30,
  COUNT(DISTINCT CASE WHEN DATEDIFF(dd, DE.@exposureStartDate, {@censor == 1} ? {LEAST(PA.dereg_date, DE.@exposureEndDate)} : {PA.dereg_date}) >= 1 THEN CAST(DE.@countEntity as varchar) + '_' + CAST(DE.@exposureConceptId as varchar) ELSE NULL END) as cx_observed_1_360,
  COUNT(DISTINCT CASE WHEN DATEDIFF(dd, DE.@exposureStartDate, {@censor == 1} ? {LEAST(PA.dereg_date, DE.@exposureEndDate)} : {PA.dereg_date}) >= 31 THEN CAST(DE.@countEntity as varchar) + '_' + CAST(DE.@exposureConceptId as varchar) ELSE NULL END) as cx_observed_31_90,
  COUNT(DISTINCT CASE WHEN DATEDIFF(dd, DE.@exposureStartDate, {@censor == 1} ? {LEAST(PA.dereg_date, DE.@exposureEndDate)} : {PA.dereg_date}) >= 91 THEN CAST(DE.@countEntity as varchar) + '_' + CAST(DE.@exposureConceptId as varchar) ELSE NULL END) as cx_observed_91_180,
  COUNT(DISTINCT CASE WHEN DATEDIFF(dd, DE.@exposureStartDate, {@censor == 1} ? {LEAST(PA.dereg_date, DE.@exposureEndDate)} : {PA.dereg_date}) >= 721 THEN CAST(DE.@countEntity as varchar) + '_' + CAST(DE.@exposureConceptId as varchar) ELSE NULL END) as cx_observed_721_1080
FROM @exposureTable DE
JOIN PERSON_OPT                PA ON DE.person_id= PA.person_id
JOIN ConceptsOfInterest       COI ON (DE.@exposureConceptId = COI.id AND COI.type = 1)

WHERE DE.drug_type_concept_id = @drugTypeConceptIdList

GROUP BY DE.@exposureConceptId
  

/*****************
BASE_LINE_COUNTER_CXY
******************/
IF OBJECT_ID('BASE_LINE_COUNTER_CXY', 'U') IS NULL 
CREATE TABLE BASE_LINE_COUNTER_CXY(
    AnalysisId              INT NOT NULL,
    @exposureConceptId      INT NOT NULL,
    @outcomeConceptId       INT NOT NULL,
    cxy_control             INT,
    cxy_1m                  INT,
    cxy_0m                  INT,
    cxy_observed_1_30       INT,
    cxy_observed_1_360 	    INT,
    cxy_observed_31_90   	  INT,
    cxy_observed_91_180  	  INT,
    cxy_observed_721_1080   INT,
    CONSTRAINT pk_BL_CXY PRIMARY KEY (AnalysisId, @exposureConceptId, @outcomeConceptId)
  )

INSERT INTO BASE_LINE_COUNTER_CXY
SELECT @analysisId, DE.@exposureConceptId, CE.@outcomeConceptId,
  COUNT(distinct CASE WHEN DATEDIFF(dd, DE.@exposureStartDate, CE.@outcomeStartDate) between @controlPeriodStart AND @controlPeriodEnd THEN CAST(DE.@countEntity as varchar) + '_' + CAST(DE.@exposureConceptId as varchar) ELSE NULL END) AS cxy_control,
  COUNT(distinct CASE WHEN DATEDIFF(dd, DE.@exposureStartDate, CE.@outcomeStartDate) between -30 AND -1                            THEN CAST(DE.@countEntity as varchar) + '_' + CAST(DE.@exposureConceptId as varchar) ELSE NULL END) AS cxy_1m,
  COUNT(distinct CASE WHEN DATEDIFF(dd, DE.@exposureStartDate, CE.@outcomeStartDate) = 0                                           THEN CAST(DE.@countEntity as varchar) + '_' + CAST(DE.@exposureConceptId as varchar) ELSE NULL END) AS cxy_0m,
  COUNT(distinct CASE WHEN DATEDIFF(dd, DE.@exposureStartDate, CE.@outcomeStartDate) between @customObservationPeriodStart AND @customObservationPeriodEnd {@censor == 1} ? {AND CE.@outcomeStartDate < DE.@exposureEndDate} : {}      THEN CAST(DE.@countEntity as varchar) + '_' + CAST(DE.@exposureConceptId as varchar) ELSE NULL END) AS cxy_observed_1_30,
  COUNT(distinct CASE WHEN DATEDIFF(dd, DE.@exposureStartDate, CE.@outcomeStartDate) between 1 AND 360 {@censor == 1} ? {AND CE.@outcomeStartDate < DE.@exposureEndDate} : {}     THEN CAST(DE.@countEntity as varchar) + '_' + CAST(DE.@exposureConceptId as varchar) ELSE NULL END) AS cxy_observed_1_360,
  COUNT(distinct CASE WHEN DATEDIFF(dd, DE.@exposureStartDate, CE.@outcomeStartDate) between 31 AND 90 {@censor == 1} ? {AND CE.@outcomeStartDate < DE.@exposureEndDate} : {}     THEN CAST(DE.@countEntity as varchar) + '_' + CAST(DE.@exposureConceptId as varchar) ELSE NULL END) AS cxy_observed_31_90,
  COUNT(distinct CASE WHEN DATEDIFF(dd, DE.@exposureStartDate, CE.@outcomeStartDate) between 91 AND 180 {@censor == 1} ? {AND CE.@outcomeStartDate < DE.@exposureEndDate} : {}    THEN CAST(DE.@countEntity as varchar) + '_' + CAST(DE.@exposureConceptId as varchar) ELSE NULL END) AS cxy_observed_91_180,
  COUNT(distinct CASE WHEN DATEDIFF(dd, DE.@exposureStartDate, CE.@outcomeStartDate) between 721 AND 1080 {@censor == 1} ? {AND CE.@outcomeStartDate < DE.@exposureEndDate} : {}  THEN CAST(DE.@countEntity as varchar) + '_' + CAST(DE.@exposureConceptId as varchar) ELSE NULL END) AS cxy_observed_721_1080

FROM @outcomeTable                     CE
JOIN @exposureTable                    DE  ON CE.person_id = DE.person_id
JOIN PERSON_OPT                        PA  ON DE.person_id= PA.person_id
JOIN LinkExposureConcept               LEC ON DE.@exposureConceptId = LEC.exposureConceptId AND CE.@outcomeConceptId = LEC.outcomeConceptId

WHERE DE.drug_type_concept_id in (@drugTypeConceptIdList)
  AND CE.condition_type_concept_id in (@conditionTypeConceptIdList)
  AND CE.@outcomeStartDate between PA.reg_date and PA.dereg_date

GROUP BY DE.@exposureConceptId, CE.@outcomeConceptId


