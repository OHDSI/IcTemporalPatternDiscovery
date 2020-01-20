--   This SQL-script is for SQLite and is a translation of the sql-server version. It was tested on the eunomia-sqlite data set. 

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
DROP TABLE IF EXISTS temp.exposure;
DROP TABLE IF EXISTS temp.all_observed;
DROP TABLE IF EXISTS temp.exposure_outcome;
DROP TABLE IF EXISTS temp.outcome;

---------------- THIS IS PART 1, CREATES EXPOSURE
CREATE TEMP TABLE exposure 
AS
SELECT a.grouping, 
       a.period_id, 
       CASE WHEN b.observed_count IS NULL THEN 0 ELSE b.observed_count END AS observed_count
FROM
(
SELECT exp_out_ids.exposure_grouping AS grouping,
    period.period_id
    FROM       (SELECT DISTINCT exposure_grouping, exposure_id FROM temp.exposure_outcome_ids) exp_out_ids
CROSS JOIN (SELECT DISTINCT period_id FROM temp.period) period
GROUP BY exp_out_ids.exposure_grouping, period.period_id 
) a
LEFT JOIN
(
SELECT exp_out_ids.exposure_grouping,
    period.period_id,
	COUNT(*) AS observed_count
FROM @exposure_database_schema.@exposure_table exposure
CROSS JOIN temp.period period
INNER JOIN @cdm_database_schema.observation_period
	ON exposure.@exposure_person_id_field = observation_period.person_id
		AND exposure.@exposure_start_field >= observation_period_start_date
		AND exposure.@exposure_start_field <= observation_period_end_date
 LEFT JOIN temp.exposure_outcome_ids exp_out_ids
	ON exp_out_ids.exposure_id = exposure.@exposure_id_field
  WHERE DATE(exposure.@exposure_start_field + 3600*24*period.period_start , 'unixepoch') <= DATE(observation_period_end_date, 'unixepoch')
  AND DATE(exposure.@exposure_start_field + 3600*24*period.period_end, 'unixepoch') >= DATE(observation_period_start_date, 'unixepoch')
{@exposure_ids != ''} ? {
	AND exposure.@exposure_id_field IN (@exposure_ids)
} : {
 {@has_pairs} ? {
 	AND exposure.@exposure_id_field IN (SELECT DISTINCT exposure_id FROM temp.exposure_outcome_ids) 
	}
}
GROUP BY exp_out_ids.exposure_grouping,
    period.period_id
) b
    ON  a.grouping = b.exposure_grouping
    AND a.period_id   = b.period_id;
    
---------------- THIS IS PART 2, CREATES EXPOSURE_OUTCOME
CREATE TEMP TABLE exposure_outcome AS 

SELECT a.exposure_grouping
     , a.outcome_grouping
	 , a.period_id
	 , CASE WHEN b.outcome_count IS NULL THEN 0 ELSE b.outcome_count END AS outcome_count
FROM
(
SELECT exposure.exposure_grouping AS exposure_grouping,
	   exp_out_ids.outcome_grouping AS outcome_grouping,
    period.period_id
FROM       (SELECT DISTINCT exposure_grouping, exposure_id, outcome_id FROM temp.exposure_outcome_ids) exposure
CROSS JOIN (SELECT DISTINCT period_id          FROM temp.period) period
CROSS JOIN (SELECT DISTINCT @outcome_id_field  FROM @outcome_database_schema.@outcome_table outcome) outcome
{@has_pairs} ? {
INNER JOIN temp.exposure_outcome_ids exp_out_ids
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
CROSS JOIN temp.period period
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
INNER JOIN temp.exposure_outcome_ids exp_out_ids
	ON exposure.@exposure_id_field = exp_out_ids.exposure_id
		AND outcome.@outcome_id_field = exp_out_ids.outcome_id
}
WHERE DATE(exposure.drug_era_start_date + 24*3600*period.period_start, 'unixepoch') <= DATE(outcome.@outcome_start_field, 'unixepoch')
	AND DATE(exposure.drug_era_start_date + 24*3600*period.period_end, 'unixepoch') >= DATE(outcome.@outcome_start_field, 'unixepoch')
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
	
---------------- THIS IS PART 3, CREATES #all_observed

	-- You could exclude the exposed from all, but as UMC prefers IC/RRR rather than PRR, 
  -- we include the exposed here. We sample the random eras per person, from all possible eras, 
  -- i.e. the exposed might contribute with a non-exposed era in all. This is done not to skew
  -- the comparison, i.e. here we compare a random sample of the exposed eras against a random sample
  -- from all eras. The same approach is used in outcome. 

CREATE TEMP TABLE all_observed 
AS	
SELECT period.period_id,
	COUNT(*) AS all_observed_count
	{@patient_level_flag} ? {
  , exposure.@exposure_person_id_field}
FROM @exposure_database_schema.@exposure_table exposure
CROSS JOIN temp.period period
INNER JOIN  @cdm_database_schema.observation_period
	ON exposure.@exposure_person_id_field = observation_period.person_id
		AND exposure.@exposure_start_field >= observation_period_start_date
		AND exposure.@exposure_start_field <= observation_period_end_date
{@random_sample_flag} ? {
LEFT JOIN temp.random_sample random_sample
  ON exposure.@exposure_person_id_field = random_sample.person_id
}

WHERE DATE(observation_period_start_date, "unixepoch") <= DATE(exposure.@exposure_start_field + 24*3600*period.period_end, "unixepoch")
	AND DATE(exposure.@exposure_start_field + 24*3600*period.period_start, "unixepoch") <= DATE(observation_period_end_date, "unixepoch")
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

-- THIS IS PART 4, CREATES OUTCOME

CREATE TEMP TABLE outcome
AS
SELECT a.grouping
	 , a.period_id
	 , CASE WHEN b.all_outcome_count IS NULL THEN 0 ELSE b.all_outcome_count END AS all_outcome_count
	 	{@patient_level_flag} ? {
   , b.@exposure_person_id_field}
FROM
(
SELECT DISTINCT exp_out_ids.outcome_grouping AS grouping,
    period.period_id
FROM       (SELECT DISTINCT outcome_grouping, outcome_id FROM temp.exposure_outcome_ids) exp_out_ids
CROSS JOIN (SELECT DISTINCT period_id FROM temp.period) period
) a
LEFT JOIN
(
SELECT exp_out_ids.outcome_grouping AS grouping,
    period.period_id,
	COUNT(*) AS all_outcome_count
		 	{@patient_level_flag} ? {
   , exposure.@exposure_person_id_field}
FROM  @exposure_database_schema.@exposure_table exposure
CROSS JOIN temp.period period
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
LEFT JOIN temp.random_sample random_sample
  ON exposure.@exposure_person_id_field = random_sample.person_id
}
LEFT JOIN temp.exposure_outcome_ids exp_out_ids
	ON exp_out_ids.outcome_id = outcome.condition_concept_id
		
WHERE DATE(exposure.@exposure_start_field + 3600*24*period.period_start, "unixepoch") <= DATE(outcome.@outcome_start_field, 'unixepoch')
	AND DATE(exposure.@exposure_start_field + 3600*24*period.period_end, "unixepoch") >= DATE(outcome.@outcome_start_field, 'unixepoch')
	{@exposure_ids != ''} ? {
	AND exposure.@exposure_id_field IN (@exposure_ids)
}	
{@outcome_ids != ''} ? {
	AND outcome.@outcome_id_field IN (@outcome_ids)
}	: {
 {@has_pairs} ? {
 	AND outcome.@outcome_id_field IN (SELECT DISTINCT outcome_id FROM temp.exposure_outcome_ids)
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
