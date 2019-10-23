--   NOTE: This is a version of CreateChronographData where groupings for outcomes and exposures are allowed, 
--   i.e. this code is called when temptable #exposure_outcome_ids contain an exposure grouping, an outcome 
--   grouping or both. This grouping allows several e.g. snomed- and rxnorm-codes to be used for calculating
--   the observed, in a single chronograph-plot.
--   Note that has_pairs is always true, when this script is used.  

{DEFAULT @exposure_ids = ''}
{DEFAULT @outcome_ids = ''}
{DEFAULT @has_pairs = TRUE}
{DEFAULT @cdm_database_schema = 'cdm.dbo'}
{DEFAULT @exposure_database_schema = 'cdm.dbo'}
{DEFAULT @exposure_table = 'drug_era'}
{DEFAULT @exposure_id_field = 'drug_concept_id'}
{DEFAULT @exposure_start_field = 'drug_era_start_date'}
{DEFAULT @exposure_person_id_field = 'person_id'}
{DEFAULT @outcome_database_schema = 'cdm.dbo'}
{DEFAULT @outcome_table = 'condition_era'}
{DEFAULT @outcome_id_field = 'condition_concept_id'}
{DEFAULT @outcome_start_field = 'condition_era_start_date'}
{DEFAULT @outcome_person_id_field = 'person_id'}

-- Drop tables if they exist
IF OBJECT_ID('tempdb..#exposure', 'U') IS NOT NULL
	DROP TABLE #exposure;

IF OBJECT_ID('tempdb..#all', 'U') IS NOT NULL
	DROP TABLE #all;	
	
IF OBJECT_ID('tempdb..#exposure_outcome', 'U') IS NOT NULL
	DROP TABLE #exposure_outcome;

IF OBJECT_ID('tempdb..#outcome', 'U') IS NOT NULL
	DROP TABLE #outcome;		

---------------- THIS PART CREATES #EXPOSURE
-- Count number of people observed relative to each exposure
SELECT a.grouping, 
       a.period_id, 
       CASE WHEN b.observed_count IS NULL THEN 0 ELSE b.observed_count END AS observed_count
INTO #exposure
FROM
(
SELECT exp_out_ids.exposure_grouping AS grouping,
    period.period_id
    FROM       (SELECT DISTINCT exposure_grouping, exposure_id FROM #exposure_outcome_ids) exp_out_ids
CROSS JOIN (SELECT DISTINCT period_id FROM #period) period
GROUP BY exp_out_ids.exposure_grouping, period.period_id
) a
LEFT JOIN
(
SELECT exp_out_ids.exposure_grouping,
    period.period_id,
	COUNT(*) AS observed_count
FROM @exposure_database_schema.@exposure_table exposure
CROSS JOIN #period period
INNER JOIN @cdm_database_schema.observation_period
	ON exposure.@exposure_person_id_field = observation_period.person_id
		AND exposure.@exposure_start_field >= observation_period_start_date
		AND exposure.@exposure_start_field <= observation_period_end_date
 LEFT JOIN #exposure_outcome_ids exp_out_ids
	ON exp_out_ids.exposure_id = exposure.@exposure_id_field
  WHERE DATEADD(DAY, period.period_start, exposure.@exposure_start_field) <= observation_period_end_date
	AND DATEADD(DAY, period.period_end, exposure.@exposure_start_field) >= observation_period_start_date
{@exposure_ids != ''} ? {
	AND exposure.@exposure_id_field IN (@exposure_ids)
} : {
 {@has_pairs} ? {
 	AND exposure.@exposure_id_field IN (SELECT DISTINCT exposure_id FROM #exposure_outcome_ids) 
	}
}	
GROUP BY exp_out_ids.exposure_grouping,
    period.period_id
) b
    ON  a.grouping = b.exposure_grouping
    AND a.period_id   = b.period_id;
	
---------------- THIS PART CREATES #ALL
-- Count number of people observed relative to any exposure

SELECT period.period_id,
	COUNT(*) AS all_observed_count
INTO #all
FROM @exposure_database_schema.@exposure_table exposure
CROSS JOIN #period period
INNER JOIN  @cdm_database_schema.observation_period
	ON exposure.@exposure_person_id_field = observation_period.person_id
		AND exposure.@exposure_start_field >= observation_period_start_date
		AND exposure.@exposure_start_field <= observation_period_end_date
WHERE DATEADD(DAY, period.period_start, exposure.@exposure_start_field) <= observation_period_end_date
	AND DATEADD(DAY, period.period_end, exposure.@exposure_start_field) >= observation_period_start_date
{@exposure_ids != ''} ? {
	AND exposure.@exposure_id_field IN (@exposure_ids)
} 
GROUP BY period.period_id;

---------------- THIS PART CREATES #EXPOSURE_OUTCOME
-- Count number of people with the outcome grouping relative to each exposure grouping (within same observation period)	
SELECT a.exposure_grouping
     , a.outcome_grouping
	 , a.period_id
	 , CASE WHEN b.outcome_count IS NULL THEN 0 ELSE b.outcome_count END AS outcome_count
INTO #exposure_outcome
FROM
(
SELECT exposure.exposure_grouping AS exposure_grouping,
	   exp_out_ids.outcome_grouping AS outcome_grouping,
    period.period_id
FROM       (SELECT DISTINCT exposure_grouping, exposure_id, outcome_id FROM #exposure_outcome_ids) exposure
CROSS JOIN (SELECT DISTINCT period_id          FROM #period) period
CROSS JOIN (SELECT DISTINCT @outcome_id_field  FROM @outcome_database_schema.@outcome_table outcome) outcome
{@has_pairs} ? {
INNER JOIN #exposure_outcome_ids exp_out_ids
	ON exposure.exposure_grouping = exp_out_ids.exposure_grouping
		AND outcome.@outcome_id_field = exp_out_ids.outcome_id
}
WHERE 1=1
{!@has_pairs} ? {
{@exposure_ids != ''} ? {
	AND exposure.@exposure_id_field IN (@exposure_ids)
}	
{@outcome_ids != ''} ? {
  AND outcome.@outcome_id_field IN (@outcome_ids)
}
}
GROUP BY exposure.exposure_grouping,
	exp_out_ids.outcome_grouping,
    period.period_id
) a
LEFT JOIN
(
SELECT exp_out_ids.outcome_grouping AS outcome_grouping,
	exp_out_ids.exposure_grouping AS exposure_grouping,
    period.period_id,
	COUNT(*) AS outcome_count
FROM @exposure_database_schema.@exposure_table exposure
CROSS JOIN #period period
INNER JOIN @outcome_database_schema.@outcome_table outcome
	ON exposure.@exposure_person_id_field = outcome.@outcome_person_id_field
INNER JOIN @cdm_database_schema.observation_period
	ON exposure.@exposure_person_id_field = observation_period.person_id
		AND exposure.@exposure_start_field >= observation_period_start_date
		AND exposure.@exposure_start_field <= observation_period_end_date
		AND outcome.@outcome_person_id_field = observation_period.person_id
		AND outcome.@outcome_start_field >= observation_period_start_date
		AND outcome.@outcome_start_field <= observation_period_end_date
{@has_pairs} ? {
INNER JOIN #exposure_outcome_ids exp_out_ids
	ON exposure.@exposure_id_field = exp_out_ids.exposure_id
		AND outcome.@outcome_id_field = exp_out_ids.outcome_id
}
WHERE DATEADD(DAY, period.period_start, exposure.@exposure_start_field) <= outcome.@outcome_start_field
	AND DATEADD(DAY, period.period_end, exposure.@exposure_start_field) >= outcome.@outcome_start_field
{!@has_pairs} ? {
{@exposure_ids != ''} ? {
	AND exposure.@exposure_id_field IN (@exposure_ids)
}
{@outcome_ids != ''} ? {
	AND outcome.@outcome_id_field IN (@outcome_ids)
}
}
GROUP BY exp_out_ids.exposure_grouping,
	exp_out_ids.outcome_grouping,
    period.period_id
) b
    ON  a.exposure_grouping = b.exposure_grouping
    AND a.outcome_grouping  = b.outcome_grouping
    AND a.period_id   = b.period_id;

-- THIS PART CREATES #OUTCOME
-- Count number of people with the outcome relative to any exposure	(within same observation period)
SELECT a.grouping
	 , a.period_id
	 , CASE WHEN b.all_outcome_count IS NULL THEN 0 ELSE b.all_outcome_count END AS all_outcome_count
INTO #outcome
FROM
(
SELECT DISTINCT exp_out_ids.outcome_grouping AS grouping,
    period.period_id
FROM       (SELECT DISTINCT outcome_grouping, outcome_id FROM #exposure_outcome_ids) exp_out_ids
CROSS JOIN (SELECT DISTINCT period_id FROM #period) period
) a
LEFT JOIN
(
SELECT exp_out_ids.outcome_grouping AS grouping,
    period.period_id,
	COUNT(*) AS all_outcome_count
FROM  @exposure_database_schema.@exposure_table exposure
CROSS JOIN #period period
INNER JOIN @outcome_database_schema.@outcome_table outcome
	ON exposure.@exposure_person_id_field = outcome.@outcome_person_id_field
INNER JOIN @cdm_database_schema.observation_period
	ON exposure.@exposure_person_id_field = observation_period.person_id
		AND exposure.@exposure_start_field >= observation_period_start_date
		AND exposure.@exposure_start_field <= observation_period_end_date
		AND outcome.@outcome_person_id_field = observation_period.person_id
		AND outcome.@outcome_start_field >= observation_period_start_date
		AND outcome.@outcome_start_field <= observation_period_end_date	
		
		LEFT JOIN #exposure_outcome_ids exp_out_ids
		ON exp_out_ids.exposure_id = exposure.drug_concept_id
		
WHERE DATEADD(DAY, period.period_start, exposure.@exposure_start_field) <= outcome.@outcome_start_field
	AND DATEADD(DAY, period.period_end, exposure.@exposure_start_field) >= outcome.@outcome_start_field
	{@exposure_ids != ''} ? {
	AND exposure.@exposure_id_field IN (@exposure_ids)
}	
{@outcome_ids != ''} ? {
	AND outcome.@outcome_id_field IN (@outcome_ids)
}	: {
 {@has_pairs} ? {
 	AND outcome.@outcome_id_field IN (SELECT DISTINCT outcome_id FROM #exposure_outcome_ids)
}}
GROUP BY exp_out_ids.outcome_grouping,
    period.period_id
) b
    ON  a.grouping  = b.grouping
    AND a.period_id   = b.period_id;
