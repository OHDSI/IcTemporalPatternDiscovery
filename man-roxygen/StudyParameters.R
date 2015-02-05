#' @param connectionDetails  An R object of type \code{ConnectionDetails} created using the function \code{createConnectionDetails} in the \code{DatabaseConnector} package.
#' @param cdmDatabaseSchema      Name of database schema that contains OMOP CDM and vocabulary.
#' @param resultsDatabaseSchema		Name of database schema that we can write results to.
#' @param exposureOutcomePairs  A data frame with at least two columns:
#' \itemize{
#'   \item{"exposureConceptId" containing the drug_concept_ID or cohort_concept_id of the exposure variable}
#'   \item{"outcomeConceptId" containing the condition_concept_ID or cohort_concept_id of the outcome variable}
#' }
#' @param exposureDatabaseSchema     The name of the database schema that is the location where the exposure data is available.  If exposureTable = DRUG_ERA, exposureSchema is not used by assumed to be cdmSchema.  Requires read permissions to this database.    
#' @param exposureTable   The tablename that contains the exposure cohorts.  If exposureTable <> DRUG_ERA, then expectation is exposureTable has format of COHORT table: COHORT_DEFINITION_ID, SUBJECT_ID, COHORT_START_DATE, COHORT_END_DATE.  
#' @param outcomeDatabaseSchema     The name of the database schema that is the location where the data used to define the outcome cohorts is available.  If exposureTable = CONDITION_ERA, exposureSchema is not used by assumed to be cdmSchema.  Requires read permissions to this database.    
#' @param outcomeTable   The tablename that contains the outcome cohorts.  If outcomeTable <> CONDITION_OCCURRENCE, then expectation is outcomeTable has format of COHORT table: COHORT_DEFINITION_ID, SUBJECT_ID, COHORT_START_DATE, COHORT_END_DATE. 	
#' @param drugTypeConceptIdList  Which drug_type to use:  generally only use 1 value (ex:  30d era).
#' @param conditionTypeConceptIdList	Which condition_type to use:  generally only use 1 value (ex:  30d era).
