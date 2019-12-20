{DEFAULT @exposure_ids = ''}
{DEFAULT @outcome_ids = ''}
{DEFAULT @cdm_database_schema = 'cdm.dbo'}
{DEFAULT @exposure_database_schema = 'cdm.dbo'}
{DEFAULT @exposure_table = 'drug_era'}
{DEFAULT @exposure_id_field = 'drug_concept_id'}
{DEFAULT @exposure_person_id_field = 'person_id'}
{DEFAULT @outcome_database_schema = 'cdm.dbo'}
{DEFAULT @outcome_table = 'condition_era'}
{DEFAULT @outcome_id_field = 'condition_concept_id'}
{DEFAULT @outcome_person_id_field = 'person_id'}

-- This is not working as intended.

-- N with exposure
UPDATE #binratio set N_exposure = (SELECT count(distinct @exposure_person_id_field) from @outcome_database_schema.@outcome_table outcome
WHERE outcome.@exposure_person_id_field in (select distinct @exposure_person_id_field from @exposure_database_schema.@exposure_table  WHERE @exposure_id_field in (@exposure_ids)))
WHERE N_exposure = 0

-- N with reaction and exposure
UPDATE #binratio set "N_exposure_outcome" = (SELECT count(distinct @outcome_person_id_field) from @outcome_database_schema.@outcome_table outcome
WHERE outcome.@outcome_id_field in (@outcome_ids) AND outcome.@outcome_person_id_field in (select distinct @exposure_person_id_field from @outcome_database_schema.@exposure_table WHERE @exposure_id_field in (@exposure_ids)))
WHERE N_exposure_outcome=0

-- Total N
UPDATE #binratio set "N_tot" = (select count(distinct @exposure_person_id_field) from @exposure_database_schema.@exposure_table)
WHERE N_tot = 0

-- N with the reaction
UPDATE #binratio set "N_outcome" = (select count(distinct @outcome_person_id_field) from @outcome_database_schema.@outcome_table outcome
WHERE outcome.@outcome_id_field in (@outcome_ids))
WHERE N_outcome = 0