# @file RunAnalyses.R
#
# Copyright 2016 Observational Health Data Sciences and Informatics
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

#' Run a list of analyses
#'
#' @details
#' Run a list of analyses for the exposure-outcomes of interest. This function will run all specified
#' analyses against all hypotheses of interest, meaning that the total number of outcome models is
#' `length(ictpdAnalysisList) * length(exposureOutcomeList)`. When you provide several analyses it
#' will determine whether any of the analyses have anything in common, and will take advantage of this
#' fact. For example, if we specify several analyses that only differ in the way the outcome model is
#' fitted, then this function will extract the data and fit the propensity model only once, and re-use
#' this in all the analysis.
#'
#' @param connectionDetails              An R object of type \code{connectionDetails} created using the
#'                                       function \code{createConnectionDetails} in the
#'                                       \code{DatabaseConnector} package.
#' @param cdmDatabaseSchema              The name of the database schema that contains the OMOP CDM
#'                                       instance. Requires read permissions to this database. On SQL
#'                                       Server, this should specifiy both the database and the schema,
#'                                       so for example 'cdm_instance.dbo'.
#' @param oracleTempSchema               For Oracle only: the name of the database schema where you
#'                                       want all temporary tables to be managed. Requires
#'                                       create/insert permissions to this database.
#' @param exposureDatabaseSchema         The name of the database schema that is the location where the
#'                                       exposure data used to define the exposure cohorts is
#'                                       available. If exposureTable = DRUG_ERA, exposureDatabaseSchema
#'                                       is not used by assumed to be cdmSchema.  Requires read
#'                                       permissions to this database.
#' @param exposureTable                  The tablename that contains the exposure cohorts.  If
#'                                       exposureTable <> DRUG_ERA, then expectation is exposureTable
#'                                       has format of COHORT table: COHORT_DEFINITION_ID, SUBJECT_ID,
#'                                       COHORT_START_DATE, COHORT_END_DATE.
#' @param outcomeDatabaseSchema          The name of the database schema that is the location where the
#'                                       data used to define the outcome cohorts is available. If
#'                                       exposureTable = CONDITION_ERA, exposureDatabaseSchema is not
#'                                       used by assumed to be cdmSchema.  Requires read permissions to
#'                                       this database.
#' @param outcomeTable                   The tablename that contains the outcome cohorts.  If
#'                                       outcomeTable <> CONDITION_OCCURRENCE, then expectation is
#'                                       outcomeTable has format of COHORT table: COHORT_DEFINITION_ID,
#'                                       SUBJECT_ID, COHORT_START_DATE, COHORT_END_DATE.
#' @param outputFolder                   Name of the folder where all the outputs will written to.
#' @param ictpdAnalysisList              A list of objects of type \code{ictpdAnalysis} as created
#'                                       using the \code{\link{createIctpdAnalysis}} function.
#' @param exposureOutcomeList            A list of objects of type \code{exposureOutcome} as created
#'                                       using the \code{\link{createExposureOutcome}} function.
#' @param cdmVersion                     Define the OMOP CDM version used: currently support "4" and
#'                                       "5".
#' @param getDbIctpdDataThreads          The number of parallel threads to use to load the data from
#'                                       the database.
#' @param calculateStatisticsIcThreads   The number of threads used to perform the IC statistics
#'                                       computations.
#'
#' @export
runIctpdAnalyses <- function(connectionDetails,
                             cdmDatabaseSchema,
                             oracleTempSchema = cdmDatabaseSchema,
                             exposureDatabaseSchema = cdmDatabaseSchema,
                             exposureTable = "drug_era",
                             outcomeDatabaseSchema = cdmDatabaseSchema,
                             outcomeTable = "condition_era",
                             cdmVersion = 4,
                             outputFolder = "./IctpdOutput",
                             ictpdAnalysisList,
                             exposureOutcomeList,
                             getDbIctpdDataThreads = 1,
                             calculateStatisticsIcThreads = 1) {
  for (exposureOutcome in exposureOutcomeList) {
    stopifnot(class(exposureOutcome) == "exposureOutcome")
  }
  for (ictpdAnalysis in ictpdAnalysisList) {
    stopifnot(class(ictpdAnalysis) == "ictpdAnalysis")
  }
  uniqueAnalysisIds <- unlist(unique(OhdsiRTools::selectFromList(ictpdAnalysisList, "analysisId")))
  if (length(uniqueAnalysisIds) != length(ictpdAnalysisList)) {
    stop("Duplicate analysis IDs are not allowed")
  }
  if (!file.exists(outputFolder))
    dir.create(outputFolder)

  ### Create reference table ###
  resultsReference <- data.frame()
  loadingArgsList <- unique(OhdsiRTools::selectFromList(ictpdAnalysisList, c("getDbIctpdDataArgs",
                                                                             "exposureType",
                                                                             "outcomeType")))
  for (i in 1:length(loadingArgsList)) {
    loadingArgs <- loadingArgsList[[i]]
    ictpdDataFile <- .createIctpdDataFileName(outputFolder, i)
    for (exposureOutcome in exposureOutcomeList) {
      exposureId <- .selectByType(loadingArgs$exposureType, exposureOutcome$exposureId, "exposure")
      outcomeId <- .selectByType(loadingArgs$outcomeType, exposureOutcome$outcomeId, "outcome")
      ictpdAnalysisSubset <- OhdsiRTools::matchInList(ictpdAnalysisList, loadingArgs)
      for (ictpdAnalysis in ictpdAnalysisSubset) {
        ictpdResultsFile <- .createIctpdResultsFileName(outputFolder, ictpdAnalysis$analysisId)

        resultsReferenceRow <- data.frame(analysisId = ictpdAnalysis$analysisId,
                                          exposureId = exposureId,
                                          outcomeId = outcomeId,
                                          ictpdDataFile = ictpdDataFile,
                                          ictpdResultsFile = ictpdResultsFile,
                                          stringsAsFactors = FALSE)
        resultsReference <- rbind(resultsReference, resultsReferenceRow)
      }
    }
  }
  saveRDS(resultsReference, file.path(outputFolder, "resultsReference.rds"))

  writeLines("*** Creating ictpdData objects ***")
  objectsToCreate <- list()
  for (ictpdDataFile in unique(resultsReference$ictpdDataFile)) {
    if (ictpdDataFile != "" && !file.exists((ictpdDataFile))) {
      refRow <- resultsReference[resultsReference$ictpdDataFile == ictpdDataFile, ][1, ]
      analysisRow <- OhdsiRTools::matchInList(ictpdAnalysisList,
                                              list(analysisId = refRow$analysisId))[[1]]
      getDbIctpdDataArgs <- analysisRow$getDbIctpdDataArgs
      # Cheap trick to find all unique combinations of exposureId and outcomeId:
      subset <- resultsReference[resultsReference$ictpdDataFile == ictpdDataFile, ]
      subset$dummy <- 1
      exposureOutcomePairs <- aggregate(dummy ~ exposureId + outcomeId, data = subset, mean)

      args <- list(connectionDetails = connectionDetails,
                   cdmDatabaseSchema = cdmDatabaseSchema,
                   oracleTempSchema = oracleTempSchema,
                   exposureDatabaseSchema = exposureDatabaseSchema,
                   exposureTable = exposureTable,
                   outcomeDatabaseSchema = outcomeDatabaseSchema,
                   outcomeTable = outcomeTable,
                   cdmVersion = cdmVersion,
                   exposureOutcomePairs = exposureOutcomePairs)
      args <- append(args, getDbIctpdDataArgs)
      objectsToCreate[[length(objectsToCreate) + 1]] <- list(args = args,
                                                             ictpdDataFile = ictpdDataFile)
    }
  }
  createIctpdDataObject <- function(params) {
    ictpdData <- do.call("getDbIctpdData", params$args)
    saveRDS(ictpdData, params$ictpdDataFile)
  }
  if (length(objectsToCreate) != 0) {
    cluster <- OhdsiRTools::makeCluster(getDbIctpdDataThreads)
    OhdsiRTools::clusterRequire(cluster, "IcTemporalPatternDiscovery")
    dummy <- OhdsiRTools::clusterApply(cluster, objectsToCreate, createIctpdDataObject)
    OhdsiRTools::stopCluster(cluster)
  }

  writeLines("*** Computing IC statistics ***")
  objectsToCreate <- list()
  for (ictpdResultsFile in unique(resultsReference$ictpdResultsFile)) {
    if (ictpdResultsFile != "" && !file.exists((ictpdResultsFile))) {
      refRow <- resultsReference[resultsReference$ictpdResultsFile == ictpdResultsFile, ][1, ]
      analysisRow <- OhdsiRTools::matchInList(ictpdAnalysisList,
                                              list(analysisId = refRow$analysisId))[[1]]
      args <- analysisRow$calculateStatisticsIcArgs
      ictpdDataFile <- refRow$ictpdDataFile

      objectsToCreate[[length(objectsToCreate) + 1]] <- list(args = args,
                                                             ictpdDataFile = ictpdDataFile,
                                                             ictpdResultsFile = ictpdResultsFile)
    }
  }
  createIctpdResults <- function(params) {
    ictpdData <- readRDS(params$ictpdDataFile)
    args <- list(ictpdData = ictpdData)
    args <- append(args, params$args)
    ictpdResults <- do.call("calculateStatisticsIc", args)
    saveRDS(ictpdResults, params$ictpdResultsFile)
  }
  if (length(objectsToCreate) != 0) {
    cluster <- OhdsiRTools::makeCluster(calculateStatisticsIcThreads)
    OhdsiRTools::clusterRequire(cluster, "IcTemporalPatternDiscovery")
    dummy <- OhdsiRTools::clusterApply(cluster, objectsToCreate, createIctpdResults)
    OhdsiRTools::stopCluster(cluster)
  }
  invisible(resultsReference)
}

.createIctpdDataFileName <- function(folder, loadingArgsId) {
  name <- paste("IctpdData_l", loadingArgsId, ".rds", sep = "")
  return(file.path(folder, name))
}

.createIctpdResultsFileName <- function(folder, analysisId) {
  name <- paste("IctpdResults_a", analysisId, ".rds", sep = "")
  return(file.path(folder, name))
}

.selectByType <- function(type, value, label) {
  if (is.null(type)) {
    if (is.list(value)) {
      stop(paste("Multiple ", label, "s specified, but none selected in analyses.", sep = ""))
    }
    return(value)
  } else {
    if (!is.list(value) || is.null(value[type])) {
      stop(paste(label, "type not found:", type))
    }
    return(value[type])
  }
}

#' Create a summary report of the analyses
#'
#' @param resultsReference   A data.frame as created by the \code{\link{runIctpdAnalyses}} function.
#'
#' @export
summarizeAnalyses <- function(resultsReference) {
  result <- data.frame()
  for (ictpdResultsFile in unique(resultsReference$ictpdResultsFile)) {
    ictpdResults <- readRDS(ictpdResultsFile)$results
    analysisId <- resultsReference$analysisId[resultsReference$ictpdResultsFile == ictpdResultsFile][1]
    ictpdResults$analysisId <- analysisId
    result <- rbind(result, ictpdResults)
  }
  return(result)
}
