{DEFAULT @has_pairs = FALSE}

TRUNCATE TABLE #observed;

DROP TABLE #observed;

TRUNCATE TABLE #all_observed;

DROP TABLE #all_observed;

TRUNCATE TABLE #outcome;

DROP TABLE #outcome;

TRUNCATE TABLE #all_outcome;

DROP TABLE #all_outcome;

TRUNCATE TABLE #period;

DROP TABLE #period;

{@has_pairs} ? {
TRUNCATE TABLE #exposure_outcome;

DROP TABLE #exposure_outcome;
}