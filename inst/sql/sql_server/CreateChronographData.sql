{DEFAULT @exposure_ids = ''}
{DEFAULT @outcome_ids = ''}
{DEFAULT @has_pairs = FALSE}
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

IF OBJECT_ID('tempdb..#observed', 'U') IS NOT NULL
	DROP TABLE #observed;

IF OBJECT_ID('tempdb..#observed_all', 'U') IS NOT NULL
	DROP TABLE #observed_all;	
	
IF OBJECT_ID('tempdb..#outcome', 'U') IS NOT NULL
	DROP TABLE #outcome;

IF OBJECT_ID('tempdb..#outcome_all', 'U') IS NOT NULL
	DROP TABLE #outcome_all;		

-- Count number of people observed relative to each exposure	
SELECT exposure.@exposure_id_field AS exposure_id,
    period.period_id,
	COUNT(*) AS observed_count
INTO #observed
FROM @exposure_database_schema.@exposure_table exposure
CROSS JOIN #period period
INNER JOIN @cdm_database_schema.observation_period
	ON exposure.@exposure_person_id_field = observation_period.person_id
		AND exposure.@exposure_start_field >= observation_period_start_date
		AND exposure.@exposure_start_field <= observation_period_end_date
WHERE DATEADD(DAY, period.period_start, exposure.@exposure_start_field) <= observation_period_end_date
	AND DATEADD(DAY, period.period_end, exposure.@exposure_start_field) >= observation_period_start_date
{@exposure_ids != ''} ? {
	AND exposure.@exposure_id_field IN (@exposure_ids)
} : {
 {@has_pairs} ? {
 	AND exposure.@exposure_id_field IN (SELECT DISTINCT exposure_id FROM #exposure_outcome)
 }
}	
GROUP BY exposure.@exposure_id_field,
    period.period_id;
	
-- Count number of people observed relative to any exposure	
SELECT period.period_id,
	COUNT(*) AS all_observed_count
INTO #all_observed
FROM @exposure_database_schema.@exposure_table exposure
CROSS JOIN #period period
INNER JOIN @cdm_database_schema.observation_period
	ON exposure.@exposure_person_id_field = observation_period.person_id
		AND exposure.@exposure_start_field >= observation_period_start_date
		AND exposure.@exposure_start_field <= observation_period_end_date
WHERE DATEADD(DAY, period.period_start, exposure.@exposure_start_field) <= observation_period_end_date
	AND DATEADD(DAY, period.period_end, exposure.@exposure_start_field) >= observation_period_start_date
{@exposure_ids != ''} ? {
	AND exposure.@exposure_id_field IN (@exposure_ids)
} 
GROUP BY period.period_id;

-- Count number of people with the outcome relative to each exposure	
SELECT exposure.@exposure_id_field AS exposure_id,
	outcome.@outcome_id_field AS outcome_id,
    period.period_id,
	COUNT(*) AS outcome_count
INTO #outcome
FROM @exposure_database_schema.@exposure_table exposure
CROSS JOIN #period period
INNER JOIN @outcome_database_schema.@outcome_table outcome
	ON exposure.@exposure_person_id_field = outcome.@outcome_person_id_field
{@has_pairs} ? {
INNER JOIN #exposure_outcome exposure_outcome
	ON exposure.@exposure_id_field = exposure_outcome.exposure_id
		AND outcome.@outcome_id_field = exposure_outcome.outcome_id
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
GROUP BY exposure.@exposure_id_field,
	outcome.@outcome_id_field,
    period.period_id;
	
-- Count number of people with the outcome relative to any exposure	
SELECT outcome.@outcome_id_field AS outcome_id,
    period.period_id,
	COUNT(*) AS all_outcome_count
INTO #all_outcome
FROM @exposure_database_schema.@exposure_table exposure
CROSS JOIN #period period
INNER JOIN @outcome_database_schema.@outcome_table outcome
	ON exposure.@exposure_person_id_field = outcome.@outcome_person_id_field
WHERE DATEADD(DAY, period.period_start, exposure.@exposure_start_field) <= outcome.@outcome_start_field
	AND DATEADD(DAY, period.period_end, exposure.@exposure_start_field) >= outcome.@outcome_start_field
{@exposure_ids != ''} ? {
	AND exposure.@exposure_id_field IN (@exposure_ids)
}	
{@outcome_ids != ''} ? {
	AND outcome.@outcome_id_field IN (@outcome_ids)
}	
GROUP BY outcome.@outcome_id_field,
    period.period_id;
