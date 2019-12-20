{DEFAULT @has_pairs = FALSE}
{DEFAULT @patient_level_flag = FALSE}

TRUNCATE TABLE #exposure;

DROP TABLE #exposure;

TRUNCATE TABLE #outcome;

DROP TABLE #outcome;

TRUNCATE TABLE #all;

DROP TABLE #all;

TRUNCATE TABLE #exposure_outcome;

DROP TABLE #exposure_outcome;

TRUNCATE TABLE #period;

DROP TABLE #period;

{@has_pairs} ? {
TRUNCATE TABLE #exposure_outcome_ids;

DROP TABLE #exposure_outcome_ids;
}

{@patient_level_flag} ? {
TRUNCATE TABLE #PatLevProb

DROP TABLE #PatLevProb

}
