-- It is known that (2019-12-02) master branch produces an #exposure with slightly fewer rows, as 0:s are not kept.
-- That code was adapted by Ola, but was apparaently not pushed to the master branch. I've noted this at the github-repo
-- for the master branch of this package. /Oskar

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
{DEFAULT @random_sample_flag = FALSE}

-- Drop tables, if they exists 
IF OBJECT_ID('tempdb..#exposure', 'U') IS NOT NULL
	DROP TABLE #exposure;

IF OBJECT_ID('tempdb..#all', 'U') IS NOT NULL
	DROP TABLE #all;	
	
IF OBJECT_ID('tempdb..#exposure_outcome', 'U') IS NOT NULL
	DROP TABLE #exposure_outcome;

IF OBJECT_ID('tempdb..#outcome', 'U') IS NOT NULL
	DROP TABLE #outcome;

---------------- THIS IS PART 1
-- Count number of exposures, e.g.:	
--          exposureId  periodId  observedCount
--              701322      -36             1
--              701322      -35             1
--              701322      -34             1
--              701322      -33             1
--              701322      -32             1

SELECT a.exposure_id, 
       a.period_id, 
       CASE WHEN b.observed_count IS NULL THEN 0 ELSE b.observed_count END AS observed_count
INTO #exposure
FROM
(
SELECT exposure.@exposure_id_field AS exposure_id,
    period.period_id
FROM       (SELECT DISTINCT @exposure_id_field FROM @exposure_database_schema.@exposure_table) exposure
CROSS JOIN (SELECT DISTINCT period_id          FROM #period) period
WHERE 1=1
{@exposure_ids != ''} ? {
	AND exposure.@exposure_id_field IN (@exposure_ids)
} : {
 {@has_pairs} ? {
 	AND exposure.@exposure_id_field IN (SELECT DISTINCT exposure_id FROM #exposure_outcome_ids)
 }
}
GROUP BY exposure.@exposure_id_field, period.period_id 
) a
LEFT JOIN
(
SELECT exposure.@exposure_id_field AS exposure_id,
  period.period_id,
	COUNT(*) AS observed_count
FROM @exposure_database_schema.@exposure_table exposure
CROSS JOIN #period period
INNER JOIN @cdm_database_schema.observation_period
	ON exposure.@exposure_person_id_field = observation_period.person_id
		AND exposure.@exposure_start_field >= observation_period_start_date
		AND exposure.@exposure_start_field <= observation_period_end_date
{@random_sample_flag} ? {
LEFT JOIN #random_sample random_sample
  ON exposure.@exposure_person_id_field = random_sample.person_id
}
WHERE DATEADD(DAY, period.period_start, exposure.@exposure_start_field) <= observation_period_end_date
	AND DATEADD(DAY, period.period_end, exposure.@exposure_start_field) >= observation_period_start_date
{@exposure_ids != ''} ? {
	AND exposure.@exposure_id_field IN (@exposure_ids)
} : {
 {@has_pairs} ? {
 	AND exposure.@exposure_id_field IN (SELECT DISTINCT exposure_id FROM #exposure_outcome_ids)
 }
}
{@random_sample_flag} ? {
 AND random_sample.included like 'TRUE'
}
GROUP BY exposure.@exposure_id_field, period.period_id 
{@random_sample_flag} ? {
 , random_sample.included
}
) b
    ON  a.exposure_id = b.exposure_id
    AND a.period_id   = b.period_id;
	
---------------- THIS IS PART 2
-- Count reports for all drugs, i.e. 
--   periodId allObservedCount
--       -36               71
--       -35               71
--       -34               71
--       -33               71
--       -32               71

SELECT period.period_id,
	COUNT(*) AS all_observed_count
INTO #all
FROM @exposure_database_schema.@exposure_table exposure
CROSS JOIN #period period
INNER JOIN @cdm_database_schema.observation_period
	ON exposure.@exposure_person_id_field = observation_period.person_id
		AND exposure.@exposure_start_field >= observation_period_start_date
		AND exposure.@exposure_start_field <= observation_period_end_date
{@random_sample_flag} ? {
LEFT JOIN #random_sample random_sample
  ON exposure.@exposure_person_id_field = random_sample.person_id
}
WHERE DATEADD(DAY, period.period_start, exposure.@exposure_start_field) <= observation_period_end_date
	AND DATEADD(DAY, period.period_end, exposure.@exposure_start_field) >= observation_period_start_date
{@exposure_ids != ''} ? {
	AND exposure.@exposure_id_field IN (@exposure_ids)
} 
{@random_sample_flag} ? {
 AND random_sample.included like 'TRUE'
}
GROUP BY period.period_id
{@random_sample_flag} ? {
 , random_sample.included
}
;

---------------- THIS IS PART 3
-- Count reports of specific reaction ("outcome") for each drug ("exposure") (within same observation period), e.g.:	
-- exposureId outcomeId periodId outcomeCount
--    1107830   4237458        8            0
--    1107830   4237458        9            0
--    1107830   4237458       10            0
--    1107830   4237458       11            0
--    1107830   4237458       12            0

SELECT a.exposure_id
     , a.outcome_id
	 , a.period_id
	 , CASE WHEN b.outcome_count IS NULL THEN 0 ELSE b.outcome_count END AS outcome_count
INTO #exposure_outcome
FROM
(
SELECT exposure.@exposure_id_field AS exposure_id,
	outcome.@outcome_id_field AS outcome_id,
    period.period_id
FROM       (SELECT DISTINCT @exposure_id_field FROM @exposure_database_schema.@exposure_table) exposure
CROSS JOIN (SELECT DISTINCT period_id          FROM #period) period
CROSS JOIN (SELECT DISTINCT @outcome_id_field  FROM @outcome_database_schema.@outcome_table outcome) outcome
{@has_pairs} ? {
INNER JOIN #exposure_outcome_ids exposure_outcome_ids
	ON exposure.@exposure_id_field = exposure_outcome_ids.exposure_id
		AND outcome.@outcome_id_field = exposure_outcome_ids.outcome_id
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
GROUP BY exposure.@exposure_id_field,
	outcome.@outcome_id_field,
    period.period_id
) a
LEFT JOIN
(
SELECT exposure.@exposure_id_field AS exposure_id,
	outcome.@outcome_id_field AS outcome_id,
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
INNER JOIN #exposure_outcome_ids exposure_outcome_ids
	ON exposure.@exposure_id_field = exposure_outcome_ids.exposure_id
		AND outcome.@outcome_id_field = exposure_outcome_ids.outcome_id
}
{@random_sample_flag} ? {
LEFT JOIN #random_sample random_sample
  ON exposure.@exposure_person_id_field = random_sample.person_id
  ON outcome.@outcome_person_id_field = exposure_outcome_ids.person_id
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
{@random_sample_flag} ? {
 AND random_sample.included like 'TRUE'
}
GROUP BY exposure.@exposure_id_field,
	outcome.@outcome_id_field,
    period.period_id
    {@random_sample_flag} ? {
 , random_sample.included
}
) b
    ON  a.exposure_id = b.exposure_id
    AND a.outcome_id  = b.outcome_id
    AND a.period_id   = b.period_id;
	
-- THIS IS PART 4
-- Count reports with specific reaction (outcome) for all drugs (exposures)	(within same observation period), e.g.
--      outcomeId   periodId allOutcomeCount
--         78272      -36               0
--         78272      -35               0
--         78272      -34               1
--         78272      -33               0
--         78272      -32               0

SELECT a.outcome_id, 
       a.period_id, 
       CASE WHEN b.all_outcome_count IS NULL THEN 0 ELSE b.all_outcome_count END AS all_outcome_count
INTO #outcome
FROM
(
SELECT outcome.@outcome_id_field AS outcome_id,
    period.period_id
FROM       (SELECT DISTINCT @outcome_id_field FROM @outcome_database_schema.@outcome_table outcome) outcome
CROSS JOIN (SELECT DISTINCT period_id         FROM #period) period
WHERE 1=1
{@outcome_ids != ''} ? {
	AND outcome.@outcome_id_field IN (@outcome_ids)
}
) a
LEFT JOIN
(
SELECT outcome.@outcome_id_field AS outcome_id,
    period.period_id,
	COUNT(*) AS all_outcome_count
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
{@random_sample_flag} ? {
LEFT JOIN #random_sample random_sample
  ON exposure.@exposure_person_id_field = random_sample.@exposure_person_id_field
}
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
GROUP BY outcome.@outcome_id_field,
    period.period_id
        {@random_sample_flag} ? {
 , random_sample.included
}
) b
    ON  a.outcome_id  = b.outcome_id
    AND a.period_id   = b.period_id;