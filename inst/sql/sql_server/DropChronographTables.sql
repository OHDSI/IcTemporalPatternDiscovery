{DEFAULT @has_pairs = FALSE}
{DEFAULT @patient_level_flag = FALSE}

DROP TABLE #exposure;

DROP TABLE #outcome;

DROP TABLE #all_observed;

DROP TABLE #exposure_outcome;

DROP TABLE #period;

{@has_pairs} ? {
DROP TABLE #exposure_outcome_ids;
}

{@patient_level_flag} ? {

DROP TABLE #PatLevProb

}
