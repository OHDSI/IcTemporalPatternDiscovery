--   This SQL-script is called from getChronographData, and creates four temporary tables.
--   It is the "workhorse" of the chronograph.
--   This particular script is a version used when an exposureOutcomePairs-table has been passed as input,
--   which results in has_pairs always being TRUE for this version of the script. 

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
{DEFAULT @random_sample_flag = FALSE}
{DEFAULT @patient_level_flag = TRUE}

-- Drop tables if they exist
IF OBJECT_ID('tempdb..#exposure', 'U') IS NOT NULL
	DROP TABLE #exposure;

IF OBJECT_ID('tempdb..#all_observed', 'U') IS NOT NULL
	DROP TABLE #all_observed;	
	
IF OBJECT_ID('tempdb..#exposure_outcome', 'U') IS NOT NULL
	DROP TABLE #exposure_outcome;

IF OBJECT_ID('tempdb..#outcome', 'U') IS NOT NULL
	DROP TABLE #outcome;

-- Each of the four parts below create two subtables, a and b, which are joined for the final temptable.
-- The a-table is just an empty frame, based on the exposures (as defined by groupings) and the periods.
-- The b-table is where patients are counted, and in the end, the results in b are merged into the a-frame.

-- The random_sample_flag only operates on the All and allOutcome-tables. It joins an include variable on the patient ID-variable,
-- and then exclude the rows where column include is not TRUE. 

---------------- THIS IS PART 1, CREATES #EXPOSURE, counts the number of observed among the exposed. 
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
    
---------------- THIS IS PART 2, CREATES #EXPOSURE_OUTCOME, also known as "the observed".
-- Count number of people with the outcome grouping relative to each exposure grouping (within each observation period)	
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
	
---------------- THIS IS PART 3, CREATES #all_observed, as in all observed, N_TOT.
  -- Counts the number of people observed for all exposures/drugs (unless a subset is specified by exposureIds)
	
	-- patient_level_flag = TRUE groups the counts by person_id. 
	
	-- A note on the randomly selected eras.
	-- You could exclude the exposed from all, but as UMC prefers IC/RRR rather than PRR, 
  -- we include the exposed here. We sample the random eras per person, from all possible eras, 
  -- i.e. the exposed might contribute with a non-exposed era in all. This is done not to skew
  -- the comparison, i.e. here we compare a random sample of the exposed eras against a random sample
  -- from all eras. The same approach is used in outcome. 
	
SELECT period.period_id,
	COUNT(*) AS all_observed_count
	{@patient_level_flag} ? {
  , exposure.@exposure_person_id_field}
INTO #all_observed
FROM @exposure_database_schema.@exposure_table exposure
CROSS JOIN #period period
INNER JOIN  @cdm_database_schema.observation_period
	ON exposure.@exposure_person_id_field = observation_period.person_id
		AND exposure.@exposure_start_field >= observation_period_start_date
		AND exposure.@exposure_start_field <= observation_period_end_date
{@random_sample_flag} ? {
LEFT JOIN #random_sample random_sample
  ON exposure.@exposure_person_id_field = random_sample.person_id
}
WHERE observation_period_start_date <= DATEADD(DAY, period.period_end, exposure.@exposure_start_field)
	AND DATEADD(DAY, period.period_start, exposure.@exposure_start_field) <= observation_period_end_date
{@exposure_ids != ''} ? {
	AND exposure.@exposure_id_field IN (@exposure_ids)
} 
{@random_sample_flag} ? {
AND random_sample.included like 'TRUE'
}
GROUP BY period.period_id
{@patient_level_flag} ? {
 , exposure.@exposure_person_id_field
}
;

-- THIS IS PART 4, CREATES #OUTCOME, also known as N_Reaction
-- Uses the whole population, to count number of outcomes relative to any exposure (within each observation period)
-- patient_level_flag = TRUE groups the counts by person_id. 

SELECT a.grouping
	 , a.period_id
	 , CASE WHEN b.all_outcome_count IS NULL THEN 0 ELSE b.all_outcome_count END AS all_outcome_count
	 	{@patient_level_flag} ? {
   , b.@exposure_person_id_field}
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
		 	{@patient_level_flag} ? {
   , exposure.@exposure_person_id_field}
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
{@random_sample_flag} ? {
LEFT JOIN #random_sample random_sample
  ON exposure.@exposure_person_id_field = random_sample.person_id
}
LEFT JOIN #exposure_outcome_ids exp_out_ids
	ON exp_out_ids.outcome_id = outcome.condition_concept_id
		
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
{@random_sample_flag} ? {
 AND random_sample.included like 'TRUE'
}
GROUP BY exp_out_ids.outcome_grouping,
    period.period_id
	 	{@patient_level_flag} ? {
   , exposure.@exposure_person_id_field}
) b
    ON  a.grouping  = b.grouping
    AND a.period_id   = b.period_id;
