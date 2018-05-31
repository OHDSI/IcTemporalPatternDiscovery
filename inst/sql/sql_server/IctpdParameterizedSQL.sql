/*****************
IC temporal pattern discovery
a Self controlled cohort design

Tomas Bergvall
******************/
{DEFAULT @cdmDatabaseSchema = 'cdm.dbo'}
{DEFAULT @exposureDatabaseSchema = 'cdm.dbo'}
{DEFAULT @exposureTable = 'drug_era'} 
{DEFAULT @exposureStartDate = 'drug_era_start_date'} 
{DEFAULT @exposureEndDate = 'drug_era_end_date'} 
{DEFAULT @exposureConceptId = 'drug_concept_id'} 
{DEFAULT @exposurePersonId = 'person_id'} 
{DEFAULT @outcomeDatabaseSchema = 'cdm.dbo'}
{DEFAULT @outcomeTable = 'condition_era'}
{DEFAULT @outcomeStartDate = 'condition_era_start_date'} 
{DEFAULT @outcomeEndDate = 'condition_era_end_date'} 
{DEFAULT @outcomeConceptId = 'condition_concept_id'} 
{DEFAULT @outcomePersonId = 'person_id'} 
{DEFAULT @controlPeriodStart = -1080}
{DEFAULT @controlPeriodEnd = -361}
{DEFAULT @riskPeriodStart = 1,
{DEFAULT @riskPeriodEnd = 30, 
{DEFAULT @censor = false}  

IF OBJECT_ID('tempdb..#PERSON_OPT', 'U') IS NOT NULL
	DROP TABLE #PERSON_OPT;
	
IF OBJECT_ID('tempdb..#BASE_LINE_COUNTER_C', 'U') IS NOT NULL
	DROP TABLE #BASE_LINE_COUNTER_C;
	
IF OBJECT_ID('tempdb..#BASE_LINE_COUNTER_CY', 'U') IS NOT NULL
	DROP TABLE #BASE_LINE_COUNTER_CY;
	
IF OBJECT_ID('tempdb..#BASE_LINE_COUNTER_CX', 'U') IS NOT NULL
	DROP TABLE #BASE_LINE_COUNTER_CX;
	
IF OBJECT_ID('tempdb..#BASE_LINE_CNTER_CXY', 'U') IS NOT NULL
	DROP TABLE #BASE_LINE_CNTER_CXY;
	
/*****************
PERSON_OPT
******************/
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
INTO #PERSON_OPT  
FROM @cdmDatabaseSchema.person PE
INNER JOIN @cdmDatabaseSchema.observation_period OP on (PE.person_id = OP.person_id)
INNER JOIN @exposureDatabaseSchema.@exposureTable      DE on (PE.person_id = DE.@exposurePersonId)
GROUP BY PE.person_id, PE.gender_concept_id, PE.year_of_birth;

/*****************
BASE_LINE_COUNTER_C
******************/
SELECT 
  COUNT(DISTINCT CASE WHEN DATEDIFF(dd, DE.@exposureStartDate, PA.reg_date) <= @controlPeriodEnd THEN CAST(DE.@exposureStartDate AS VARCHAR) + '_' + CAST(DE.@exposureConceptId AS VARCHAR) ELSE NULL END) as c_control,
  COUNT(DISTINCT CASE WHEN DATEDIFF(dd, DE.@exposureStartDate, PA.reg_date) <= -1                THEN CAST(DE.@exposureStartDate AS VARCHAR) + '_' + CAST(DE.@exposureConceptId AS VARCHAR) ELSE NULL END) as c_1m,
  COUNT(DISTINCT CASE WHEN DATEDIFF(dd, DE.@exposureStartDate, PA.reg_date) <= 0                 THEN CAST(DE.@exposureStartDate AS VARCHAR) + '_' + CAST(DE.@exposureConceptId AS VARCHAR) ELSE NULL END) as c_0m,
  COUNT(DISTINCT CASE WHEN DATEDIFF(dd, DE.@exposureStartDate, {@censor} ? {CASE WHEN PA.dereg_date < DE.@exposureEndDate THEN PA.dereg_date ELSE DE.@exposureEndDate END} : {PA.dereg_date}) >= @riskPeriodStart           THEN CAST(DE.@exposureStartDate AS VARCHAR) + '_' + CAST(DE.@exposureConceptId AS VARCHAR) ELSE NULL END) as c_observed_1_30,
  COUNT(DISTINCT CASE WHEN DATEDIFF(dd, DE.@exposureStartDate, {@censor} ? {CASE WHEN PA.dereg_date < DE.@exposureEndDate THEN PA.dereg_date ELSE DE.@exposureEndDate END} : {PA.dereg_date}) >= 1           THEN CAST(DE.@exposureStartDate AS VARCHAR) + '_' + CAST(DE.@exposureConceptId AS VARCHAR) ELSE NULL END) as c_observed_1_360,
  COUNT(DISTINCT CASE WHEN DATEDIFF(dd, DE.@exposureStartDate, {@censor} ? {CASE WHEN PA.dereg_date < DE.@exposureEndDate THEN PA.dereg_date ELSE DE.@exposureEndDate END} : {PA.dereg_date}) >= 31          THEN CAST(DE.@exposureStartDate AS VARCHAR) + '_' + CAST(DE.@exposureConceptId AS VARCHAR) ELSE NULL END) as c_observed_31_90,
  COUNT(DISTINCT CASE WHEN DATEDIFF(dd, DE.@exposureStartDate, {@censor} ? {CASE WHEN PA.dereg_date < DE.@exposureEndDate THEN PA.dereg_date ELSE DE.@exposureEndDate END} : {PA.dereg_date}) >= 91          THEN CAST(DE.@exposureStartDate AS VARCHAR) + '_' + CAST(DE.@exposureConceptId AS VARCHAR) ELSE NULL END) as c_observed_91_180,
  COUNT(DISTINCT CASE WHEN DATEDIFF(dd, DE.@exposureStartDate, {@censor} ? {CASE WHEN PA.dereg_date < DE.@exposureEndDate THEN PA.dereg_date ELSE DE.@exposureEndDate END} : {PA.dereg_date}) >= 721         THEN CAST(DE.@exposureStartDate AS VARCHAR) + '_' + CAST(DE.@exposureConceptId AS VARCHAR) ELSE NULL END) as c_observed_721_1080
INTO #BASE_LINE_COUNTER_C
FROM @exposureDatabaseSchema.@exposureTable DE
JOIN #PERSON_OPT              PA ON (DE.@exposurePersonId = PA.person_id);

/*****************
BASE_LINE_COUNTER_CY
******************/

SELECT CE.@outcomeConceptId,
  COUNT(DISTINCT CASE WHEN DATEDIFF(dd, DE.@exposureStartDate, CE.@outcomeStartDate) between @controlPeriodStart AND @controlPeriodEnd THEN CAST(DE.@exposureStartDate AS VARCHAR) + '_' + CAST(DE.@exposureConceptId AS VARCHAR) ELSE NULL END) AS cy_control,
  COUNT(DISTINCT CASE WHEN DATEDIFF(dd, DE.@exposureStartDate, CE.@outcomeStartDate) between -30 AND -1 THEN CAST(DE.@exposureStartDate AS VARCHAR) + '_' + CAST(DE.@exposureConceptId AS VARCHAR) ELSE NULL END) AS cy_1m,
  COUNT(DISTINCT CASE WHEN DATEDIFF(dd, DE.@exposureStartDate, CE.@outcomeStartDate) = 0 THEN CAST(DE.@exposureStartDate AS VARCHAR) + '_' + CAST(DE.@exposureConceptId AS VARCHAR) ELSE NULL END) AS cy_0m,
  COUNT(DISTINCT CASE WHEN DATEDIFF(dd, DE.@exposureStartDate, CE.@outcomeStartDate) between @riskPeriodStart AND @riskPeriodEnd {@censor} ? {AND CE.@outcomeStartDate < DE.@exposureEndDate} : {} THEN CAST(DE.@exposureStartDate AS VARCHAR) + '_' + CAST(DE.@exposureConceptId AS VARCHAR) ELSE NULL END) AS cy_observed_1_30,
  COUNT(DISTINCT CASE WHEN DATEDIFF(dd, DE.@exposureStartDate, CE.@outcomeStartDate) between 1 AND 360 {@censor} ? {AND CE.@outcomeStartDate < DE.@exposureEndDate} : {} THEN CAST(DE.@exposureStartDate AS VARCHAR) + '_' + CAST(DE.@exposureConceptId AS VARCHAR) ELSE NULL END) AS cy_observed_1_360,
  COUNT(DISTINCT CASE WHEN DATEDIFF(dd, DE.@exposureStartDate, CE.@outcomeStartDate) between 31 AND 90 {@censor} ? {AND CE.@outcomeStartDate < DE.@exposureEndDate} : {} THEN CAST(DE.@exposureStartDate AS VARCHAR) + '_' + CAST(DE.@exposureConceptId AS VARCHAR) ELSE NULL END) AS cy_observed_31_90,
  COUNT(DISTINCT CASE WHEN DATEDIFF(dd, DE.@exposureStartDate, CE.@outcomeStartDate) between 91 AND 180 {@censor} ? {AND CE.@outcomeStartDate < DE.@exposureEndDate} : {} THEN CAST(DE.@exposureStartDate AS VARCHAR) + '_' + CAST(DE.@exposureConceptId AS VARCHAR) ELSE NULL END) AS cy_observed_91_180,
  COUNT(DISTINCT CASE WHEN DATEDIFF(dd, DE.@exposureStartDate, CE.@outcomeStartDate) between 721 AND 1080 {@censor} ? {AND CE.@outcomeStartDate < DE.@exposureEndDate} : {} THEN CAST(DE.@exposureStartDate AS VARCHAR) + '_' + CAST(DE.@exposureConceptId AS VARCHAR) ELSE NULL END) AS cy_observed_721_1080
INTO #BASE_LINE_COUNTER_CY
FROM @outcomeDatabaseSchema.@outcomeTable   CE
JOIN @exposureDatabaseSchema.@exposureTable  DE ON (CE.@outcomePersonId = DE.@exposurePersonId)
JOIN #concepts_of_interest                   COI ON (CE.@outcomeConceptId = COI.id AND COI.type = 2)
JOIN #PERSON_OPT                              PA  ON (DE.@exposurePersonId = PA.person_id)
WHERE CE.@outcomeStartDate between PA.reg_date and PA.dereg_date
GROUP BY CE.@outcomeConceptId;

/*****************
BASE_LINE_COUNTER_CX
******************/

SELECT DE.@exposureConceptId, 
  COUNT(DISTINCT CASE WHEN DATEDIFF(dd, DE.@exposureStartDate, PA.reg_date) <= @controlPeriodEnd THEN CAST(DE.@exposureStartDate AS VARCHAR) + '_' + CAST(DE.@exposureConceptId AS VARCHAR) ELSE NULL END) as cx_control,
  COUNT(DISTINCT CASE WHEN DATEDIFF(dd, DE.@exposureStartDate, PA.reg_date) <= -1                THEN CAST(DE.@exposureStartDate AS VARCHAR) + '_' + CAST(DE.@exposureConceptId AS VARCHAR) ELSE NULL END) as cx_1m,
  COUNT(DISTINCT CASE WHEN DATEDIFF(dd, DE.@exposureStartDate, PA.reg_date) <= 0                 THEN CAST(DE.@exposureStartDate AS VARCHAR) + '_' + CAST(DE.@exposureConceptId AS VARCHAR) ELSE NULL END) as cx_0m,
  COUNT(DISTINCT CASE WHEN DATEDIFF(dd, DE.@exposureStartDate, {@censor} ? {CASE WHEN PA.dereg_date < DE.@exposureEndDate THEN PA.dereg_date ELSE DE.@exposureEndDate END} : {PA.dereg_date}) >= @riskPeriodStart THEN CAST(DE.@exposureStartDate AS VARCHAR) + '_' + CAST(DE.@exposureConceptId AS VARCHAR) ELSE NULL END) as cx_observed_1_30,
  COUNT(DISTINCT CASE WHEN DATEDIFF(dd, DE.@exposureStartDate, {@censor} ? {CASE WHEN PA.dereg_date < DE.@exposureEndDate THEN PA.dereg_date ELSE DE.@exposureEndDate END} : {PA.dereg_date}) >= 1 THEN CAST(DE.@exposureStartDate AS VARCHAR) + '_' + CAST(DE.@exposureConceptId AS VARCHAR) ELSE NULL END) as cx_observed_1_360,
  COUNT(DISTINCT CASE WHEN DATEDIFF(dd, DE.@exposureStartDate, {@censor} ? {CASE WHEN PA.dereg_date < DE.@exposureEndDate THEN PA.dereg_date ELSE DE.@exposureEndDate END} : {PA.dereg_date}) >= 31 THEN CAST(DE.@exposureStartDate AS VARCHAR) + '_' + CAST(DE.@exposureConceptId AS VARCHAR) ELSE NULL END) as cx_observed_31_90,
  COUNT(DISTINCT CASE WHEN DATEDIFF(dd, DE.@exposureStartDate, {@censor} ? {CASE WHEN PA.dereg_date < DE.@exposureEndDate THEN PA.dereg_date ELSE DE.@exposureEndDate END} : {PA.dereg_date}) >= 91 THEN CAST(DE.@exposureStartDate AS VARCHAR) + '_' + CAST(DE.@exposureConceptId AS VARCHAR) ELSE NULL END) as cx_observed_91_180,
  COUNT(DISTINCT CASE WHEN DATEDIFF(dd, DE.@exposureStartDate, {@censor} ? {CASE WHEN PA.dereg_date < DE.@exposureEndDate THEN PA.dereg_date ELSE DE.@exposureEndDate END} : {PA.dereg_date}) >= 721 THEN CAST(DE.@exposureStartDate AS VARCHAR) + '_' + CAST(DE.@exposureConceptId AS VARCHAR) ELSE NULL END) as cx_observed_721_1080
INTO #BASE_LINE_COUNTER_CX 
FROM @exposureDatabaseSchema.@exposureTable DE
JOIN #PERSON_OPT                PA ON DE.@exposurePersonId = PA.person_id
JOIN #concepts_of_interest      COI ON (DE.@exposureConceptId = COI.id AND COI.type = 1)
{@drugTypeConceptIdList != '' & cdm_version == 4} ? {
WHERE DE.drug_type_concept_id = @drugTypeConceptIdList
}
GROUP BY DE.@exposureConceptId;
  

/*****************
BASE_LINE_COUNTER_CXY
******************/
SELECT DE.@exposureConceptId, CE.@outcomeConceptId,
  COUNT(DISTINCT CASE WHEN DATEDIFF(dd, DE.@exposureStartDate, CE.@outcomeStartDate) between @controlPeriodStart AND @controlPeriodEnd THEN CAST(DE.@exposureStartDate AS VARCHAR) + '_' + CAST(DE.@exposureConceptId AS VARCHAR) ELSE NULL END) AS cxy_control,
  COUNT(DISTINCT CASE WHEN DATEDIFF(dd, DE.@exposureStartDate, CE.@outcomeStartDate) between -30 AND -1                            THEN CAST(DE.@exposureStartDate AS VARCHAR) + '_' + CAST(DE.@exposureConceptId AS VARCHAR) ELSE NULL END) AS cxy_1m,
  COUNT(DISTINCT CASE WHEN DATEDIFF(dd, DE.@exposureStartDate, CE.@outcomeStartDate) = 0                                           THEN CAST(DE.@exposureStartDate AS VARCHAR) + '_' + CAST(DE.@exposureConceptId AS VARCHAR) ELSE NULL END) AS cxy_0m,
  COUNT(DISTINCT CASE WHEN DATEDIFF(dd, DE.@exposureStartDate, CE.@outcomeStartDate) between @riskPeriodStart AND @riskPeriodEnd {@censor} ? {AND CE.@outcomeStartDate < DE.@exposureEndDate} : {}      THEN CAST(DE.@exposureStartDate AS VARCHAR) + '_' + CAST(DE.@exposureConceptId AS VARCHAR) ELSE NULL END) AS cxy_observed_1_30,
  COUNT(DISTINCT CASE WHEN DATEDIFF(dd, DE.@exposureStartDate, CE.@outcomeStartDate) between 1 AND 360 {@censor} ? {AND CE.@outcomeStartDate < DE.@exposureEndDate} : {}     THEN CAST(DE.@exposureStartDate AS VARCHAR) + '_' + CAST(DE.@exposureConceptId AS VARCHAR) ELSE NULL END) AS cxy_observed_1_360,
  COUNT(DISTINCT CASE WHEN DATEDIFF(dd, DE.@exposureStartDate, CE.@outcomeStartDate) between 31 AND 90 {@censor} ? {AND CE.@outcomeStartDate < DE.@exposureEndDate} : {}     THEN CAST(DE.@exposureStartDate AS VARCHAR) + '_' + CAST(DE.@exposureConceptId AS VARCHAR) ELSE NULL END) AS cxy_observed_31_90,
  COUNT(DISTINCT CASE WHEN DATEDIFF(dd, DE.@exposureStartDate, CE.@outcomeStartDate) between 91 AND 180 {@censor} ? {AND CE.@outcomeStartDate < DE.@exposureEndDate} : {}    THEN CAST(DE.@exposureStartDate AS VARCHAR) + '_' + CAST(DE.@exposureConceptId AS VARCHAR) ELSE NULL END) AS cxy_observed_91_180,
  COUNT(DISTINCT CASE WHEN DATEDIFF(dd, DE.@exposureStartDate, CE.@outcomeStartDate) between 721 AND 1080 {@censor} ? {AND CE.@outcomeStartDate < DE.@exposureEndDate} : {}  THEN CAST(DE.@exposureStartDate AS VARCHAR) + '_' + CAST(DE.@exposureConceptId AS VARCHAR) ELSE NULL END) AS cxy_observed_721_1080
INTO #BASE_LINE_CNTER_CXY
FROM @outcomeDatabaseSchema.@outcomeTable       CE
JOIN @exposureDatabaseSchema.@exposureTable     DE  ON CE.@outcomePersonId = DE.@exposurePersonId
JOIN #PERSON_OPT                        PA  ON DE.@exposurePersonId = PA.person_id
JOIN #exposure_outcome               LEC ON DE.@exposureConceptId = LEC.exposure_concept_id AND CE.@outcomeConceptId = LEC.outcome_concept_id
WHERE CE.@outcomeStartDate between PA.reg_date and PA.dereg_date
GROUP BY DE.@exposureConceptId, CE.@outcomeConceptId;
