{DEFAULT @has_pairs = FALSE}

TRUNCATE TABLE #exposure;

DROP TABLE #exposure;

TRUNCATE TABLE #outcome;

DROP TABLE #outcome;

TRUNCATE TABLE #all_exposures;

DROP TABLE #all_exposures;

TRUNCATE TABLE #exposure_outcome;

DROP TABLE #exposure_outcome;

TRUNCATE TABLE #period;

DROP TABLE #period;

{@has_pairs} ? {
TRUNCATE TABLE #exposure_outcome_ids;

DROP TABLE #exposure_outcome_ids;
}
