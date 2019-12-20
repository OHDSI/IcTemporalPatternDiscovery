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

IF OBJECT_ID('tempdb..#all', 'U') IS NOT NULL
	DROP TABLE #all;	
	
IF OBJECT_ID('tempdb..#exposure_outcome', 'U') IS NOT NULL
	DROP TABLE #exposure_outcome;

IF OBJECT_ID('tempdb..#outcome', 'U') IS NOT NULL
	DROP TABLE #outcome;

---------------- THIS IS PART 1, CREATES #EXPOSURE
-- Counts the number of observed patients (or eras, depending on patient_level-parameter) for each exposure/reaction.

-- Each of the four parts below create two subtables, a and b, which are joined for the final temptable.
-- The a-table is just an empty frame, based on the exposures (as defined by groupings) and the periods.
-- The b-table is where patients are counted, and in the end, the results in b are merged into the a-frame.

-- The random_sample_flag and patient_level_flag works very similar, they join on TRUE-variables to some ID-variable,
-- and then exclude the rows which lack the TRUE. 

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
	{@random_sample_flag} ? {
 LEFT JOIN #random_sample random_sample
  ON random_sample.person_id = exposure.@exposure_person_id_field
  }
  	{@patient_level_flag} ? {
   LEFT JOIN #exposed_eras_patlev eras_patlev
  ON eras_patlev.DRUG_ERA_ID = exposure.drug_era_id
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
{@patient_level_flag} ? {
 AND eras_patlev.selected_era like 'TRUE'
}
GROUP BY exp_out_ids.exposure_grouping,
    period.period_id
{@random_sample_flag} ? {
 , random_sample.included
}
) b
    ON  a.grouping = b.exposure_grouping
    AND a.period_id   = b.period_id;
	
---------------- THIS IS PART 2, CREATES #ALL
  -- Counts the number of people observed for all exposures/drugs (unless a subset is specified by exposureIds)
	-- Position e.g. -36 to 36 months centered around the drug era initiation date. 
	-- For each period, check if there is at least one day within the observation period
	-- of the patient. This means that for this period, the patient/drug-combination has
	-- been able to experience the event i.e. this is the denominator.
	
	-- A note on the randomly selected eras.
	-- Note that you could exclude the exposed from all, but as UMC prefers IC/RRR rather than PRR, 
  -- we include the exposed here. We sample the random eras per person, from all possible eras, 
  -- i.e. the exposed might contribute with a non-exposed era in all. This is done not to skew
  -- the comparison, i.e. here we compare a random sample of the exposed eras against a random sample
  -- from all eras. The same approach is used in outcome. 
	
SELECT period.period_id,
	COUNT(*) AS all_observed_count
INTO #all
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
{@patient_level_flag} ? {
  -- Note that we use the second randomly selected era-dataframe here.
   LEFT JOIN #of_all_eras_patlev eras_patlev
  ON eras_patlev.DRUG_ERA_ID = exposure.drug_era_id
  }
WHERE observation_period_start_date <= DATEADD(DAY, period.period_end, exposure.@exposure_start_field)
	AND DATEADD(DAY, period.period_start, exposure.@exposure_start_field) <= observation_period_end_date
{@exposure_ids != ''} ? {
	AND exposure.@exposure_id_field IN (@exposure_ids)
} 
{@random_sample_flag} ? {
AND random_sample.included like 'TRUE'
}
{@patient_level_flag} ? {
 AND eras_patlev.selected_era like 'TRUE'
}
GROUP BY period.period_id
{@random_sample_flag} ? {
 , random_sample.included
}
;

---------------- THIS IS PART 3, CREATES #EXPOSURE_OUTCOME
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
{@random_sample_flag} ? {
LEFT JOIN #random_sample random_sample
  ON exposure.@exposure_person_id_field = random_sample.person_id
  AND outcome.@outcome_id_field = exp_out_ids.outcome_id
}
{@patient_level_flag} ? {
LEFT JOIN #exposed_eras_patlev eras_patlev
  ON eras_patlev.DRUG_ERA_ID = exposure.drug_era_id
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
{@patient_level_flag} ? {
 AND eras_patlev.selected_era like 'TRUE'
}
GROUP BY exp_out_ids.exposure_grouping,
	exp_out_ids.outcome_grouping,
    period.period_id
{@random_sample_flag} ? {
 , random_sample.included
}
) b
    ON  a.exposure_grouping = b.exposure_grouping
    AND a.outcome_grouping  = b.outcome_grouping
    AND a.period_id   = b.period_id;

-- THIS IS PART 4, CREATES #OUTCOME
-- Uses the whole population, to count number of outcomes relative to any exposure (within each observation period)
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
{@random_sample_flag} ? {
LEFT JOIN #random_sample random_sample
  ON exposure.@exposure_person_id_field = random_sample.person_id
}
{@patient_level_flag} ? {
  -- Note that we use the second randomly selected era-dataframe here.
LEFT JOIN #of_all_eras_patlev eras_patlev
  ON eras_patlev.DRUG_ERA_ID = exposure.drug_era_id
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
{@patient_level_flag} ? {
 AND eras_patlev.selected_era like 'TRUE'
}
GROUP BY exp_out_ids.outcome_grouping,
    period.period_id
            {@random_sample_flag} ? {
 , random_sample.included
}
) b
    ON  a.grouping  = b.grouping
    AND a.period_id   = b.period_id;
