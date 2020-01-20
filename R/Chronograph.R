# Copyright 2019 Observational Health Data Sciences and Informatics
#
# This file is part of IcTemporalPatternDiscovery
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# TO DO LIST
# Implement the unit tests for the chronograph using Eunomia, testthat and Travis CI
# Merge the "sample-one-drug-era"-part back again, and run speed tests.
# Decide where to go with the power calculation/variance-test.
# Merge the SQL-scripts so that there is just one.
# Write a Vignette

#  This script contains the following functions:
#  getChronographData
#  plotChronograph

#' @title
#' Get the data for chronographs from the server.
#'
#' @description
#' Get the data for creating chronographs from the server.
#'
#' @param connectionDetails        An R object of type \code{ConnectionDetails} created using the
#'                                 function \code{createConnectionDetails} in the
#'                                 \code{DatabaseConnector} package.
#' @param cdmDatabaseSchema        Name of database schema that contains OMOP CDM and vocabulary.
#' @param oracleTempSchema         For Oracle only: the name of the database schema where you want all
#'                                 temporary tables to be managed. Requires create/insert permissions
#'                                 to this database.
#' @param cdmVersion               Define the OMOP CDM version used: currently supports "5" and "6".
#' @param exposureIds              A vector of IDs identifying the exposures to include when computing 
#'                                 the expected count, i.e. the supplied IDs will define the 
#'                                 background population in the chronograph. If the
#'                                 exposure table is the drug_era table, these IDs correspond to
#'                                 ingredient concept IDs. If the exposure table has the format of the
#'                                 cohort table, these IDs correspond to the cohort definition IDs. If
#'                                 left empty, all records in the exposure table will be used.
#' @param outcomeIds               A vector of IDs identifying the outcomes to include when computing 
#'                                 the expected count, i.e. the supplied IDs will define the 
#'                                 background population in the chronograph. If the outcome
#'                                 table is the drug_era table, these IDs correspond to condition
#'                                 concept IDs. If the outcomes table has the format of the cohort
#'                                 table, these IDs correspond to the cohort definition IDs. If left
#'                                 empty, all records in the outcome table will be used.
#' @param exposureOutcomePairs              A data frame with at least two columns:
#'                                          \itemize{
#'                                            \item {"exposureId" containing the drug_concept_ID or
#'                                                  cohort_concept_id of the exposure variable}
#'                                            \item {"outcomeId" containing the condition_concept_ID or
#'                                                  cohort_concept_id of the outcome variable}
#'                                          } Each row specifies an exposure-outcome-combination for 
#'                                 which to collect data for. If left empty, all possible combinations of exposures and
#'                                 outcomes will be computed, which could be time consuming.
#'                                 To collect data for plotting multiple exposures and outcomes in a single
#'                                  chronograph, an additional "grouping" column might be passed: 
#'                                          \itemize{
#'                                            \item {"grouping" with the same value at each row with an exposure and exposure to
#'                                                  include in a grouping. See details and examples below.}
#'                                 }
#'                                 If no grouping column is passed, a grouping column of 1:nrow(exposureOutcomePairs) is created.
#' @param exposureDatabaseSchema   The name of the database schema that is the location where the
#'                                 exposure data is available.  If exposureTable = DRUG_ERA,
#'                                 exposureSchema is not used by assumed to be cdmSchema.  Requires
#'                                 read permissions to this database.
#' @param exposureTable            The tablename that contains the exposure cohorts.  If exposureTable
#'                                 <> DRUG_ERA, then expectation is exposureTable has format of COHORT
#'                                 table: COHORT_DEFINITION_ID, SUBJECT_ID, COHORT_START_DATE,
#'                                 COHORT_END_DATE.
#' @param outcomeDatabaseSchema    The name of the database schema that is the location where the data
#'                                 used to define the outcome cohorts is available. If exposureTable =
#'                                 CONDITION_ERA, exposureSchema is not used by assumed to be
#'                                 cdmSchema.  Requires read permissions to this database.
#' @param outcomeTable             The tablename that contains the outcome cohorts.  If outcomeTable <>
#'                                 CONDITION_OCCURRENCE, then expectation is outcomeTable has format of
#'                                 COHORT table: COHORT_DEFINITION_ID, SUBJECT_ID, COHORT_START_DATE,
#'                                 COHORT_END_DATE.
#' @param shrinkage                Shrinkage used in IRR calculations, required >0 to deal with 0 case
#'                                 counts, but larger number means more shrinkage. default is 0.5
#' @param icPercentile             The lower bound of the credibility interval for the IC values
#'                                 (IClow). default is 0.025,
#' @param randomSample             The chronograph can, if time is an issue, be run on a random subset of 
#'                                 the database with sample size specified by this parameter. If NULL, 
#'                                 all records are used.
#' @param patientLevel             Defaults to TRUE. Estimation can be made on era-level or patient-level. 
#'                                 A detailed description is planned in a vignette.
#'
#' @details  The chronograph displays the observed and the expected number of persons with the event over time. The approach can be 
#' described as self-controlled in the sense that the observed number of persons with the reaction before and after the exposure can be
#' compared. But the chronograph also allows comparison against the expected, which could be all persons with a drug initiation, or some
#' subset of those, as controlled by the outcomeIds and exposureIds. 
#'
#' @details  The chronograph displays the observed and the expected number of persons with the event over time. The approach can be 
#' described as self-controlled in the sense that the observed number of persons with the reaction before and after the exposure can be
#' compared. But the chronograph also allows comparison against the expected, which could be all persons with a drug initiation, or some
#' subset of those, as controlled by the outcomeIds and exposureIds. 
#' 
#' When mapping vocabularies e.g. from medDRA to SNOMED, it is possible that the mapping produces multiple terms. To create a 
#' chronograph for the joint SNOMED-terms, data collected from server for single exposures and outcomes cannot not be aggregated 
#' later for multiple exposures or multiple outcomes, as the same person might be counted multiple times. You should instead pass an 
#' exposureOutcomePairs-table, when the interest lies in a chronograph based on several SNOMED or RxNormExtended-terms.   
#' @return
#'  A dataframe named results with observed and expected outcome counts in periods relative to the exposure
#' initiation date, for each outcome and exposure.
#'
#' @export
getChronographData <- function(connectionDetails,
                               cdmDatabaseSchema,
                               oracleTempSchema = NULL,
                               cdmVersion = "5",
                               exposureIds = c(),
                               outcomeIds = c(),
                               exposureOutcomePairs = NULL,
                               exposureDatabaseSchema = cdmDatabaseSchema,
                               exposureTable = "drug_era",
                               outcomeDatabaseSchema = cdmDatabaseSchema,
                               outcomeTable = "condition_era",
                               shrinkage = 0.5,
                               icPercentile = 0.025,
                               randomSample = NULL,
                               patientLevel = TRUE) {
  
  # Debugging parameters for testing
  
  # # Eunomia
  library(Eunomia)
  connectionDetails <- getEunomiaConnectionDetails()
  conn <- connect(connectionDetails)
  cdmDatabaseSchema <- c("cdm_synthea10", "mini","main")[3]
  
  # These exposures and outcomes are in eunomia
  exposureIds = c(738818)
  outcomeIds = c(260139)
  exposureOutcomePairs = data.frame("exposureId" = exposureIds, "outcomeId" = outcomeIds)
  exposureOutcomePairs$grouping = 1
  cdmVersion = 5
  
  ## These parameters should always be set
  # exposureOutcomePairs = data.frame("exposureId" = exposureIds, "outcomeId" = outcomeIds)

  exposureDatabaseSchema = cdmDatabaseSchema
  exposureTable = "drug_era"
  outcomeDatabaseSchema = cdmDatabaseSchema
  outcomeTable = "condition_era"
  shrinkage = 0.5
  icPercentile = 0.025
  oracleTempSchema = NULL
  exposureIds = NULL
  randomSample = 10^5
  patientLevel = T
  
  # # Birth control and Myocardial Infarction
  # Synthea
  # connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = "sql server",  server = "UMCDB06", schema = "OmopCdmSynthea")
  # cdmVersion = "6"
  # exposureIds = 1549786
  # outcomeIds = 4329847
  # ## Acetaminophen and Acute Bronchitis
  # exposureIds = 1125315
  # outcomeIds = 260139
  
  start <- Sys.time()
  
  ##############################################
  # Setup and modify input parameters depending on input
  ##############################################
  conn <- DatabaseConnector::connect(connectionDetails)
  on.exit(DatabaseConnector::disconnect(conn))
  
  # Convert parameter names to lower case ----
  exposureTable <- tolower(exposureTable)
  outcomeTable <- tolower(outcomeTable)
  
  # Depending on exposureTable being drug era or not, ----
  # assign additional parameter values
  if (exposureTable == "drug_era") {
    exposureStartField <- "drug_era_start_date"
    
    # If version 6, drug_era_start is now drug_era_start_datetime:
    if(cdmVersion == 6) {
      exposureStartField = "drug_era_start_datetime"
    }
    exposureIdField <- "drug_concept_id"
    exposurePersonIdField <- "person_id"
  } else {
    exposureStartField <- "cohort_start_date"
    exposureIdField <- "cohort_definition_id"
    exposurePersonIdField <- "subject_id"
  }
  if (outcomeTable == "condition_era") {
    outcomeStartField <- "condition_era_start_date"
    
    # If version 6, condition_era_start is now drug_era_starttime:
    if(cdmVersion == 6) {
      outcomeStartField = "condition_era_start_datetime"
    }
    outcomeIdField <- "condition_concept_id"
    outcomePersonIdField <- "person_id"
  } else {
    outcomeStartField <- "cohort_start_date"
    
    # If version 6, drug_era_start is now drug_era_starttime:
    if(cdmVersion == 6) {
      outcomeStartField = "cohort_start_datetime"
    }
    outcomeIdField <- "cohort_definition_id"
    outcomePersonIdField <- "subject_id"
  }
  
  ##########################################
  # Insert the #period-table into the db
  ##########################################
  # Create a df where each time period is a row ("periodsForDb") ----
  periodLength <- 30
  numberOfPeriods <- 72
  periodStarts <- c(
    seq(-periodLength*numberOfPeriods/2L, -1L, by=periodLength),
    0L,
    seq(1L, 1L + periodLength * (-1L + numberOfPeriods/2L), by=periodLength)
  )
  periodEnds <- periodStarts + periodLength - 1L
  periodEnds[periodStarts==0L] <- 0L
  periods <- data.frame(periodStart = periodStarts,
                        periodEnd = periodEnds,
                        periodId = c((-numberOfPeriods/2L):(-1L),0L,1L:(numberOfPeriods/2L)))
  periodsForDb <- periods
  colnames(periodsForDb) <- SqlRender::camelCaseToSnakeCase(colnames(periodsForDb))
  
  ParallelLogger::logTrace("Inserting tables of IDs")
  DatabaseConnector::insertTable(connection = conn,
                                 tableName = "#period",
                                 data = periodsForDb,
                                 dropTableIfExists = TRUE,
                                 createTable = TRUE,
                                 tempTable = T,
                                 oracleTempSchema = oracleTempSchema)
  
  ###########################################################
  # Take care of groupings, if such have been passed
  ########################################################### 
  # Insert an exposure-outcome-temptable if exposureOutcomePairs exists ----
  if (is.null(exposureOutcomePairs)) {
    hasPairs <- FALSE
    sql_script = "CreateChronographDataNoExposureOutcomePairs.sql"
    
  } else {
    hasPairs <- TRUE
  
    sql_script = "CreateChronographDataFromExposureOutcomePairs.sql"
    
    # Just for the eunomia-test
    if(connectionDetails$dbms == "sqlite"){
      sql_script = "sqlite.sql"
    }
    
    # Check whether exposureOutcomePairs was passed with a grouping variable.
    # If not, we create one.
    if(!any(grepl("group", colnames(exposureOutcomePairs), ignore.case = T))){
      exposureOutcomePairs$grouping = 1:nrow(exposureOutcomePairs)
    }
    
    # Copy the groupings into two (code could be rewritten to only use one single variable, 
    # but currently it expects two grouping variables, one for exposure and one for outcome.)
    exposureOutcomePairs$exposureGrouping = exposureOutcomePairs$grouping
    exposureOutcomePairs$outcomeGrouping = exposureOutcomePairs$grouping
    exposureOutcomePairs$grouping = NULL
    
    # Insert exposureOutcomePairs in the database
    colnames(exposureOutcomePairs) <- SqlRender::camelCaseToSnakeCase(colnames(exposureOutcomePairs))
    DatabaseConnector::insertTable(conn,
                                   "#exposure_outcome_ids",
                                   exposureOutcomePairs,
                                   tempTable = T,
                                   dropTableIfExists = TRUE)
  }
  
  #############################################################################
  # Only use a randomly sampled part of the db, if randomSample has been passed.
  ##############################################################################
  # There did not exist a db-independent way of sampling rows randomly, hence
  # we use randomSample from R. 
  
  if(!is.null(randomSample)){
    
    #Based on testing on sqllite and sql server
    path_to_table <- ifelse(connectionDetails$dbms == "sql server", paste0(as.character(c(connectionDetails$schema, cdmDatabaseSchema)), collapse="."), cdmDatabaseSchema)
    
    sql <- paste0("select person_id from ", path_to_table, ".person")
    
    sql <- SqlRender::translate(sql,
                                targetDialect = connectionDetails$dbms,
                                oracleTempSchema = oracleTempSchema)
    
    all <- querySql(conn, sql)
    # If randomSample is larger than the data set
    if(nrow(all)<randomSample){
      ParallelLogger::logInfo("Size of random sample exceeds full data set => Full data set is used.")
      randomSampleFlag = F
      randomSample = NULL
      # If randomSample is not larger than the data set
    } else {
      randomSampleFlag = T
      randomSampleDf <- setNames(as.data.frame(all[sample(nrow(all), randomSample),]), "person_id")
      randomSampleDf$included = T
      DatabaseConnector::insertTable(conn,
                                     "#random_sample",
                                     randomSampleDf,
                                     tempTable = T,
                                     dropTableIfExists = TRUE,
                                     oracleTempSchema = oracleTempSchema)
      
    }
    
    # If randomSample was null:
  } else {
    randomSampleFlag = F
  }
  
  ###############################################################################################################
  # Run workhorse SQL-script CreateChronographData, where four temptables (#outcome, #exposure, ...) are created
  ###############################################################################################################
  
  SqlRender::loadRenderTranslateSql(sql_script, dbms="sqlite", packageName="IcTemporalPatternDiscovery")
  
  sql <- SqlRender::loadRenderTranslateSql(sqlFilename = sql_script,
                                           packageName = "IcTemporalPatternDiscovery",
                                           dbms = connectionDetails$dbms,
                                           oracleTempSchema = oracleTempSchema,
                                           cdm_database_schema = cdmDatabaseSchema,
                                           exposure_database_schema = exposureDatabaseSchema,
                                           exposure_table = exposureTable,
                                           exposure_id_field = exposureIdField,
                                           exposure_start_field = exposureStartField,
                                           exposure_person_id_field = exposurePersonIdField,
                                           outcome_database_schema = outcomeDatabaseSchema,
                                           outcome_table = outcomeTable,
                                           outcome_id_field = outcomeIdField,
                                           outcome_start_field = outcomeStartField,
                                           outcome_person_id_field = outcomePersonIdField,
                                           exposure_ids = exposureIds,
                                           outcome_ids = outcomeIds,
                                           has_pairs = hasPairs,
                                           random_sample_flag = randomSampleFlag,
                                           patient_level_flag = patientLevel)
  
  ParallelLogger::logInfo("Creating counts on server")
  DatabaseConnector::executeSql(conn, sql)
  # Too see the rendered query with linebreaks:
  # writeLines(sql)
  
  ###############################
  # Extract the four temptables
  ###############################
  ParallelLogger::logInfo("Loading data server")
  
  # EXPOSURE and EXPOSUREOUTCOME always use the full database
  # EXPOSURE 
  sql <- "SELECT * FROM #exposure"
  sql <- SqlRender::translate(sql,
                              targetDialect = connectionDetails$dbms,
                              oracleTempSchema = oracleTempSchema)
  exposure <- DatabaseConnector::querySql(conn, sql)
  colnames(exposure)[1] = ifelse(hasPairs, "exposure_grouping", "exposure_id")
  colnames(exposure) <- SqlRender::snakeCaseToCamelCase(colnames(exposure))
  
  # EXPOSURE_OUTCOME
  # Get outcomes-count for exposed for all periods ----
  sql <- "SELECT * FROM #exposure_outcome"
  sql <- SqlRender::translate(sql,
                              targetDialect = connectionDetails$dbms,
                              oracleTempSchema = oracleTempSchema)
  exposureOutcome <- DatabaseConnector::querySql(conn, sql)
  colnames(exposureOutcome) <- SqlRender::snakeCaseToCamelCase(colnames(exposureOutcome))
  
  # If drug-level is wanted, extract all the counts for each unique era in ALL and EXPOSURE_OUTCOME
  if(!patientLevel){
    # ALL
    sql <- "SELECT * FROM #all_observed"
    sql <- SqlRender::translate(sql,
                                targetDialect = connectionDetails$dbms,
                                oracleTempSchema = oracleTempSchema)
    all <- DatabaseConnector::querySql(conn, sql)
    colnames(all) <- SqlRender::snakeCaseToCamelCase(colnames(all))
    
    # ALL_OUTCOME
    # Get total outcomes-count, for all periods ----
    sql <- "SELECT * FROM #outcome"
    sql <- SqlRender::translate(sql,
                                targetDialect = connectionDetails$dbms,
                                oracleTempSchema = oracleTempSchema)
    outcome <- DatabaseConnector::querySql(conn, sql)
    colnames(outcome)[1] = ifelse(hasPairs, "exposure_grouping", "outcome_id")
    colnames(outcome) <- SqlRender::snakeCaseToCamelCase(colnames(outcome))
  }
  
  # Select output variables depending on groups or not
  if(hasPairs){
    granularity = "grouping"
  } else {
    granularity = c("exposureId", "outcomeId")}
  
  # Define function to calculate IC
  ic_calc <- function(obs, exp, shape.add = 0.5, rate.add = 0.5, percentile = 0.025) {
    ic <- log2((obs + shape.add)/(exp + rate.add))
    ic_low <- log2(qgamma(p = percentile, shape = (obs + shape.add), rate = (exp + rate.add)))
    ic_high <- log2(qgamma(p = (1 - percentile), shape = (obs + shape.add), rate = (exp + rate.add)))
    return(list(ic = ic, ic_low = ic_low, ic_high = ic_high))
  }
  
  if(patientLevel){
    # Merge the All and allOutcome by period and person_ID, take the average probability for each patient, and then average.
    sql <- paste0("DROP TABLE IF EXISTS #PatLevProb; ",
                  "SELECT a.period_id, a.person_id, o.grouping, 
                  CASE WHEN o.all_outcome_count IS NULL THEN 0 ELSE o.all_outcome_count END AS all_outcome_count, 
                  a.all_observed_count 
                  INTO #PatLevProb FROM #outcome o 
                  INNER JOIN #all_observed a ON a.period_id=o.period_id AND a.person_id = o.person_id;")
    
    sql <- SqlRender::translate(sql,
                                targetDialect = connectionDetails$dbms,
                                oracleTempSchema = oracleTempSchema)
    DatabaseConnector::executeSql(conn, sql)
    #writeLines(sql)
    
    sql <- "SELECT grouping, period_id, SUM(all_outcome_count),  SUM(all_observed_count), AVG(cast(all_outcome_count as float)/cast(all_observed_count as float)) FROM #PatLevProb GROUP BY period_id, grouping"
    sql <- SqlRender::translate(sql,
                                targetDialect = connectionDetails$dbms,
                                oracleTempSchema = oracleTempSchema)
    
    reaction_probability <- DatabaseConnector::querySql(conn, sql)
    reaction_probability <- reaction_probability[order(reaction_probability$GROUPING, reaction_probability$PERIOD_ID),]
    
    if(connectionDetails$dbms=="sqlite"){
      colnames(reaction_probability) = c("grouping","periodId","allOutcomeCount","allObservedCount","reactionProbability")
    } else {
      colnames(reaction_probability) <- SqlRender::snakeCaseToCamelCase(colnames(reaction_probability))
    }
    
    exposureOutcome$grouping <- exposureOutcome$exposureGrouping
    result <- merge(reaction_probability, exposureOutcome, by=c("periodId", "grouping"))
    result <- merge(result, exposure, by=c("periodId","exposureGrouping"))
    result$expectedCount = result$observedCount * result$reactionProbability
    
    ic <- ic_calc(obs = result$outcomeCount,
                  exp = result$expectedCount,
                  shape.add = shrinkage,
                  rate.add = shrinkage,
                  percentile = icPercentile)
    
    result$ic <- ic$ic
    result$icLow <- ic$ic_low
    result$icHigh <- ic$ic_high
    
    result <- result[, c("grouping", colnames(result)[-3])]
    result <- result[order(result$periodId),]
    result <- result[, c(granularity, "periodId", "observedCount", "allOutcomeCount", "allObservedCount",
                         "reactionProbability", "expectedCount", "outcomeCount", "ic", "icLow", "icHigh")]
  } else {
    # Gather the results, calculate ic (by function ic found in ICTPD.R: ----
    result <- merge(all, exposure)
    result <- merge(result, outcome)
    result <- merge(exposureOutcome, result)
    
    # Clean up the double grouping-variables
    if(all(result$outcomeGrouping == result$exposureGrouping)){
      result$grouping = result$outcomeGrouping
      result$exposureGrouping = NULL
      result$outcomeGrouping = NULL
    } else {
      stop("The SQL-script CreateChronographDataFromExposureOutcomePairs has returned invalid grouping-variables.")
    }
    
    # Calculate expected count as in RRR (include the drug of interest in the baseline probability)
    result$expectedCount <- result$observedCount * result$allOutcomeCount/result$allObservedCount
    
    ic <- ic_calc(obs = result$outcomeCount,
                  exp = result$expectedCount,
                  shape.add = shrinkage,
                  rate.add = shrinkage,
                  percentile = icPercentile)
    
    result$ic <- ic$ic
    result$icLow <- ic$ic_low
    result$icHigh <- ic$ic_high
    
    # Order according to grouping/exposureId, periodId
    if(hasPairs){
      result <- result[order(result$grouping, result$periodId, decreasing = F),]
    } else {
      result <- result[order(result$exposureId, result$outcomeId, result$periodId, decreasing = F),]
    }
    
    result$reactionProbability = result$expectedCount/(result$observedCount)
    result <- result[, c(granularity, "periodId", "observedCount", "allOutcomeCount", "allObservedCount",
                         "reactionProbability", "expectedCount", "outcomeCount", "ic", "icLow", "icHigh")]
  }
  
  ############################################
  # Clean-up, remove temptables, calculate IC
  ############################################
  sql <- SqlRender::loadRenderTranslateSql(sqlFilename = "DropChronographTables.sql",
                                           packageName = "IcTemporalPatternDiscovery",
                                           dbms = connectionDetails$dbms,
                                           oracleTempSchema = oracleTempSchema,
                                           has_pairs = hasPairs,
                                           patient_level_flag = patientLevel)
  DatabaseConnector::executeSql(conn, sql, progressBar = FALSE, reportOverallTime = FALSE)
  
  delta <- Sys.time() - start
  ParallelLogger::logInfo(paste("Getting data took", signif(delta, 3), attr(delta, "units")))
  
  # cat(randomSample)
  # IcTemporalPatternDiscovery::plotChronograph(result, grouping=1)
  # IcTemporalPatternDiscovery::plotChronograph(result, grouping=2)
  
  result[result$grouping==1,]
  
  return(result)
}

#' @title
#' Plot a chronograph
#'
#' @description
#' Creates a plot showing the observed and expected nunber of outcomes for each month in the 3 years
#' before and after initiation of the exposure, as well as the IC. The full process is described in
#' Noren et al.
#'
#' @param data         Data as generated using the \code{getChronographData} function.
#' @param exposureId   The unique ID identifying the exposure to plot.
#' @param outcomeId    The unique ID identifying the outcome to plot.
#' @param grouping     The unique grouping ID, in case \code{getChronographData} was called with groupings.
#' @param removeZero   Don't plot the value at time zero. 
#' @param title        The title to show above the plot.
#' @param fileName     Name of the file where the plot should be saved, for example 'plot.png'. See the
#'                     function \code{ggsave} in the ggplot2 package for supported file formats.
#'
#' @references
#' Noren GN, Hopstadius J, Bate A, Star K, Edwards R, Temporal pattern discovery in longitudinal
#' electronic patient records, Data Mining and Knowledge Discovery, May 2010, Volume 20, Issue 3, pp
#' 361-387.
#'
#' @export
plotChronograph <- function(data, exposureId, outcomeId, grouping = NULL, 
                            removeZero=FALSE, title = NULL, fileName = NULL) {
  
  # grouping = 1
  # data <- result
  
  if(removeZero){
    data <- data[,data$periodId!=0]
  }
  
  if(all(!is.null(grouping))){
    data <- data[data$grouping %in% grouping,]
  } else {
    data <- data[data$exposureId == exposureId & data$outcomeId == outcomeId, ]
  }
  
  # Cut up data depending on which period they occured in ----
  negData <- data[data$periodId < 0, ]
  posData <- data[data$periodId > 0, ]
  zeroData <- data[data$periodId == 0, ]
  
  # Set plotting parameters depending on the calculated ic-values ----
  if (max(data$icHigh) + 0.5 < 1) {
    yMax <- 1
  } else {
    yMax <- max(data$icHigh) + 0.1
  }
  if (min(data$icLow) - 0.5 > -1) {
    yMin <- -1
  } else {
    yMin <- min(data$icLow) - 0.1
  }
  
  # Create top panel ---- 
  topPlot <- with(data, ggplot2::ggplot() +
                    ggplot2::geom_hline(yintercept = 0, color = "black", size = 0.2, linetype = 2) +
                    ggplot2::geom_errorbar(ggplot2::aes(x = periodId, ymax = icHigh, ymin = icLow),
                                           color = "grey50",
                                           size = 0.35,
                                           data = negData) +
                    ggplot2::geom_errorbar(ggplot2::aes(x = periodId, ymax = icHigh, ymin = icLow),
                                           color = "grey50",
                                           size = 0.35,
                                           data = posData) +
                    ggplot2::geom_line(ggplot2::aes(x = periodId, y = ic),
                                       color = rgb(0, 0, 0.8),
                                       size = 0.7,
                                       data = negData) +
                    ggplot2::geom_line(ggplot2::aes(x = periodId, y = ic),
                                       color = rgb(0, 0, 0.8),
                                       size = 0.7,
                                       data = posData) +
                    ggplot2::geom_point(ggplot2::aes(x = periodId, y = ic),
                                        color = rgb(0, 0, 0.8),
                                        size = 6,
                                        shape = "*",
                                        data = zeroData) +
                    ggplot2::scale_x_continuous(name = "Months relative to first prescription",
                                                breaks = (-5:5) * 12) +
                    ggplot2::ylab("IC") +
                    ggplot2::coord_cartesian(ylim = c(yMin, yMax)) +
                    ggplot2::theme(axis.title.x = ggplot2::element_blank())
  )
  
  # Create bottom panel ---- 
  bottomPlot <- with(data, ggplot2::ggplot() +
                       ggplot2::geom_bar(ggplot2::aes(x = periodId, y = outcomeCount, fill = "Observed"),
                                         stat = "identity",
                                         color = "black",
                                         size = 0.4,
                                         width = 1,
                                         data = data) +
                       ggplot2::geom_line(ggplot2::aes(x = periodId,
                                                       y = expectedCount,
                                                       color = "Expected"), size = 0.7, data = negData) +
                       ggplot2::geom_line(ggplot2::aes(x = periodId, y = expectedCount),
                                          color = rgb(0, 0, 0.8),
                                          size = 0.7,
                                          data = posData) +
                       ggplot2::geom_point(ggplot2::aes(x = periodId, y = expectedCount),
                                           color = rgb(0, 0, 0.8),
                                           size = 6,
                                           shape = "*",
                                           data = zeroData) +
                       ggplot2::scale_x_continuous(name = "Months relative to first exposure",
                                                   breaks = (-5:5) * 12) +
                       ggplot2::scale_y_continuous(name="Number of outcomes",
                                                   breaks = function(x) {
                                                     if (all(x==0)) { x <- c(0,1) }
                                                     else           { x <- c(0,x) }
                                                     out <- unique(ceiling(pretty(x)))
                                                     return(out)
                                                   }) +
                       ggplot2::scale_fill_manual(name = "", values = c(rgb(0.3, 0.7, 0.8, alpha = 0.5))) +
                       ggplot2::scale_color_manual(name = "", values = c(rgb(0, 0, 0.8))) +
                       ggplot2::coord_cartesian(ylim = c(0L, max(data$outcomeCount, data$expectedCount, 1L))) +
                       ggplot2::theme(panel.grid.minor.y = ggplot2::element_blank(),
                                      legend.justification = c(1, 1),
                                      legend.position = c(1, 1),
                                      legend.direction = "horizontal",
                                      legend.box = "vertical",
                                      legend.key.height = ggplot2::unit(0.4, units = "lines"),
                                      legend.key = ggplot2::element_rect(fill = "transparent", color = NA),
                                      legend.background = ggplot2::element_rect(fill = "white", color = "black", size = 0.2))
  )
  
  # Put the two panels together and plot ---- 
  plots <- list(topPlot, bottomPlot)
  grobs <- widths <- list()
  for (i in 1:length(plots)) {
    grobs[[i]] <- ggplot2::ggplotGrob(plots[[i]])
    widths[[i]] <- grobs[[i]]$widths[2:5]
  }
  maxwidth <- do.call(grid::unit.pmax, widths)
  for (i in 1:length(grobs)) {
    grobs[[i]]$widths[2:5] <- as.list(maxwidth)
  }
  plot <- gridExtra::grid.arrange(grobs[[1]], grobs[[2]], top = grid::textGrob(title))
  
  # Possibly save the plot ----
  if (!is.null(fileName))
    ggplot2::ggsave(fileName, plot, width = 7, height = 5, dpi = 400)
  return(plot)
}
