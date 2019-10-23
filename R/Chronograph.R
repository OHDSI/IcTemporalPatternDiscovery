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
#' @param cdmVersion               Define the OMOP CDM version used: currently supports "5".
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
#' @param returnSQL                Set to True to also return the rendered sql-query which creates the 
#'                                 four central temptables, useful for debugging purposes.
#'
#'
#'
#' @examples   
#' 
#' # Collect data with a specific "background" (we don't use the default - i.e. the entire database as a baseline) 
#' 
#' exposureOutcomePairs <- cbind.data.frame("exposureId"=c(40170549), "outcomeId"=c(321389))
#' exposure_background <- c(1321636, 40167333, 711452, 40173533, 981774, 19030860, 19037401, 925102, 1337068) 
#' outcome_background <- c(73609,  134401,   74725,  375251,  199063,  381580, 4046804,  442094,  436883)
#'           
#'  output <- getChronographData(connectionDetails = connectionDetails,
#'                               oracleTempSchema = NULL,
#'                               cdmDatabaseSchema = "YOUR_DATABASE_SCHEMA",
#'                               cdmVersion = "5",
#'                               exposureIds = exposure_background,
#'                               outcomeIds = outcome_background,
#'                               exposureOutcomePairs = exposureOutcomePairs,
#'                               exposureDatabaseSchema = cdmDatabaseSchema,
#'                               exposureTable = "drug_era",
#'                               outcomeDatabaseSchema = cdmDatabaseSchema,
#'                               outcomeTable = "condition_era",
#'                               shrinkage = 0.5,
#'                               icPercentile = 0.025)
#'                               
#'  plot(IcTemporalPatternDiscovery::plotChronograph(data=output, exposureId = 40170549, outcomeId = 321389, title="Exposure 40170549 vs Outcome 321389"))
#' 
#' 
#' # Collect data for multiple exposures and multiple outcomes (grouping 1), as well as for single rows (grouping 2 and 3)
#' 
#' grouping_df <- cbind.data.frame("grouping"=1, setNames(expand.grid(c(40170549, 1321636), c(321389,   73609,  134401)), c("exposureId", "outcomeId")))
#' single_combinations_df <- cbind.data.frame("grouping"=2:3, "exposureId"=c(40170549, 1321636), "outcomeId"=c(321389,   73609))
#' exposureOutcomePairs <- rbind.data.frame(grouping_df, single_combinations_df)
#'           
#'  output <- getChronographData(connectionDetails = connectionDetails,
#'                               oracleTempSchema = NULL,
#'                               cdmDatabaseSchema = "YOUR_DATABASE_SCHEMA",
#'                               cdmVersion = "5",
#'                               exposureOutcomePairs = exposureOutcomePairs,
#'                               exposureDatabaseSchema = cdmDatabaseSchema,
#'                               exposureTable = "drug_era",
#'                               outcomeDatabaseSchema = cdmDatabaseSchema,
#'                               outcomeTable = "condition_era",
#'                               shrinkage = 0.5,
#'                               icPercentile = 0.025)
#'  
#'  plot(IcTemporalPatternDiscovery::plotChronograph(data=output, exposureGrouping = 1, outcomeGrouping = 1, title="Exposures 40170549, 1321636 vs Outcomes 321389, 73609, 134401"))
#'  
#'
#'
#' @details  When mapping vocabularies e.g. from medDRA to SNOMED, it is possible that the mapping produces multiple terms. To create a chronograph for the joint SNOMED-terms, 
#'           data collected from server for single exposures and outcomes should not be used for multiple exposures or multiple outcomes, 
#'           as the same person might be counted multiple times. You should instead specify groupings in a column named "grouping" in the 
#'           exposureOutcomePairs-argument table. See the examples. 
#' @return
#' If returnSQL is False: A dataframe named results with observed and expected outcome counts in periods relative to the exposure
#' initiation date, for each outcome and exposure.
#' If returnSQL is True: A list containing 1) data frame results 2) The sql-query that creates the four central temptables, for debugging
#' purposes. 
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
                               returnSQL = F) {
  start <- Sys.time()
  
  # Convert parameter names to lower case ----
  exposureTable <- tolower(exposureTable)
  outcomeTable <- tolower(outcomeTable)
  
  # Depending on exposureTable being drug era or not, ----
  # assign additional parameter values
  if (exposureTable == "drug_era") {
    exposureStartField <- "drug_era_start_date"
    exposureIdField <- "drug_concept_id"
    exposurePersonIdField <- "person_id"
  } else {
    exposureStartField <- "cohort_start_date"
    exposureIdField <- "cohort_definition_id"
    exposurePersonIdField <- "subject_id"
  }
  if (outcomeTable == "condition_era") {
    outcomeStartField <- "condition_era_start_date"
    outcomeIdField <- "condition_concept_id"
    outcomePersonIdField <- "person_id"
  } else {
    outcomeStartField <- "cohort_start_date"
    outcomeIdField <- "cohort_definition_id"
    outcomePersonIdField <- "subject_id"
  }
  
  # Create a df where each time period is a row ("periodsForDb"), ---
  # which is inserted into the db as a temptable #period
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
  
  # Connect to database ----
  conn <- DatabaseConnector::connect(connectionDetails)
  on.exit(DatabaseConnector::disconnect(conn))
  
  ParallelLogger::logTrace("Inserting tables of IDs")
  DatabaseConnector::insertTable(connection = conn,
                                 tableName = "#period",
                                 data = periodsForDb,
                                 dropTableIfExists = TRUE,
                                 createTable = TRUE,
                                 tempTable = TRUE,
                                 oracleTempSchema = oracleTempSchema)
  
  # Check whether exposureOutcomePairs was passed with a grouping variable.
  grouping_flag <- any(grepl("group", colnames(exposureOutcomePairs), ignore.case = T))
  
  # In that case, grouping is copied into two new columns. 
  if(grouping_flag){
    exposureOutcomePairs$exposureGrouping = exposureOutcomePairs$grouping
    exposureOutcomePairs$outcomeGrouping = exposureOutcomePairs$grouping
    exposureOutcomePairs$grouping = NULL
  }
  
  
  # Create an exposure-outcome-tempdb if exposureOutcomePairs is not null ----
  if (is.null(exposureOutcomePairs)) {
    hasPairs <- FALSE
  } else {
    hasPairs <- TRUE
    colnames(exposureOutcomePairs) <- SqlRender::camelCaseToSnakeCase(colnames(exposureOutcomePairs))
    DatabaseConnector::insertTable(conn,
                                   "#exposure_outcome_ids",
                                   exposureOutcomePairs,
                                   tempTable = TRUE,
                                   dropTableIfExists = TRUE)
  }
  
  
  # If exposureOutcomePairs has grouping, run special version of createChronographData
  sql_filename <- ifelse(grouping_flag, 
                         "CreateChronographDataWithGroupings.sql",
                         "CreateChronographData.sql")
  
  
  # run SQL-script CreateChronographData, where several temptables (#outcome, #exposure, ...), ----
  # are created
  sql <- SqlRender::loadRenderTranslateSql(sqlFilename = sql_filename,
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
                                           has_pairs = hasPairs)
  
  sql_query_to_return <- sql
  ParallelLogger::logInfo("Creating counts on server")
  DatabaseConnector::executeSql(conn, sql)
  
  # Get observed count, for all periods ----
  ParallelLogger::logInfo("Loading data server")
  sql <- "SELECT * FROM #exposure"
  sql <- SqlRender::translate(sql,
                              targetDialect = connectionDetails$dbms,
                              oracleTempSchema = oracleTempSchema)
  exposure <- DatabaseConnector::querySql(conn, sql)
  if(grouping_flag){
    colnames(exposure)[1] = "exposure_grouping"
  }
  colnames(exposure) <- SqlRender::snakeCaseToCamelCase(colnames(exposure))
  
  # Get total count, for all periods ---- 
  sql <- "SELECT period_id, all_observed_count FROM #all"
  sql <- SqlRender::translate(sql,
                              targetDialect = connectionDetails$dbms,
                              oracleTempSchema = oracleTempSchema)
  all <- DatabaseConnector::querySql(conn, sql)
  colnames(all) <- SqlRender::snakeCaseToCamelCase(colnames(all))
  
  # Get outcomes-count for exposed for all periods ----
  sql <- "SELECT * FROM #exposure_outcome"
  sql <- SqlRender::translate(sql,
                              targetDialect = connectionDetails$dbms,
                              oracleTempSchema = oracleTempSchema)
  exposureOutcome <- DatabaseConnector::querySql(conn, sql)
  colnames(exposureOutcome) <- SqlRender::snakeCaseToCamelCase(colnames(exposureOutcome))
  
  # Get total outcomes-count, for all periods ----   
  sql <- "SELECT * FROM #outcome"
  sql <- SqlRender::translate(sql,
                              targetDialect = connectionDetails$dbms,
                              oracleTempSchema = oracleTempSchema)
  outcome <- DatabaseConnector::querySql(conn, sql)
  if(grouping_flag){
    colnames(outcome)[1] = "outcome_grouping"
  }
  colnames(outcome) <- SqlRender::snakeCaseToCamelCase(colnames(outcome))
  
  # Cleanup, drop temptables and so on
  sql <- SqlRender::loadRenderTranslateSql(sqlFilename = "DropChronographTables.sql",
                                           packageName = "IcTemporalPatternDiscovery",
                                           dbms = connectionDetails$dbms,
                                           oracleTempSchema = oracleTempSchema,
                                           has_pairs = hasPairs)
  DatabaseConnector::executeSql(conn, sql, progressBar = FALSE, reportOverallTime = FALSE)
  
  # Gather the results, calculate ic (by function ic found in ICTPD.R: ----
  result <- merge(all, exposure)
  result <- merge(result, outcome)
  result <- merge(exposureOutcome, result)
  result$expectedCount <- result$observedCount * result$allOutcomeCount/result$allObservedCount
  ic <- IcTemporalPatternDiscovery:::ic(obs = result$outcomeCount,
           exp = result$expectedCount,
           shape.add = shrinkage,
           rate.add = shrinkage,
           percentile = icPercentile)
  
  result$ic <- ic$ic
  result$icLow <- ic$ic_low
  result$icHigh <- ic$ic_high
  
  sql <- sql_query_to_return
  
  delta <- Sys.time() - start
  ParallelLogger::logInfo(paste("Getting data took", signif(delta, 3), attr(delta, "units")))
  
  if(returnSQL){
  return(list("result"=result, "sql"=writeLines(sql)))
  } else {
    return(result)
  }
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
plotChronograph <- function(data, exposureId, outcomeId, exposureGrouping=NULL, outcomeGrouping=NULL, title = NULL, fileName = NULL) {
  
  if(all(!is.null(c(exposureGrouping, outcomeGrouping)))){
    data <- data[data$exposureGrouping %in% exposureGrouping & data$outcomeGrouping %in% outcomeGrouping,]
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
