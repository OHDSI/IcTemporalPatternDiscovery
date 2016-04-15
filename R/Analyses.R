# @file Analyses.R
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

#' Create ICTPD analysis details
#'
#' @description
#' \code{createIctpdAnalysis} generates an object specifying one set of analysis choices for the IC
#' Temporal Pattern Discovery method.
#'
#' @param analysisId                  A unique identifier that can later be used to identify the
#'                                    results of this analysis
#' @param description                 A short description of the analysis.
#' @param exposureType                If more than one exposure is provided for each exposureOutcome,
#'                                    this field should be used to select the specific exposure to use
#'                                    in this analysis.
#' @param outcomeType                 If more than one outcome is provided for each exposureOutcome,
#'                                    this field should be used to select the specific outcome to use
#'                                    in this analysis.
#' @param getDbIctpdDataArgs          An object representing the arguments to be used when calling the
#'                                    \code{\link{getDbIctpdData}} function.
#' @param calculateStatisticsIcArgs   An object representing the arguments to be used when calling the
#'                                    \code{\link{calculateStatisticsIc}} function.
#'
#' @export
createIctpdAnalysis <- function(analysisId = 1,
                                description = "",
                                exposureType = NULL,
                                outcomeType = NULL,
                                getDbIctpdDataArgs,
                                calculateStatisticsIcArgs) {
  ictpdAnalysis <- OhdsiRTools::convertArgsToList(match.call(), "ictpdAnalysis")
  return(ictpdAnalysis)
}

#' Save a list of ictpdAnalysis to file
#'
#' @description
#' Write a list of objects of type \code{ictpdAnalysis} to file. The file is in JSON format.
#'
#' @param ictpdAnalysisList   The ictpdAnalysis list to be written to file
#' @param file                The name of the file where the results will be written
#'
#' @export
saveIctpdAnalysisList <- function(ictpdAnalysisList, file) {
  stopifnot(is.list(ictpdAnalysisList))
  stopifnot(length(ictpdAnalysisList) > 0)
  for (i in 1:length(ictpdAnalysisList)) {
    stopifnot(class(ictpdAnalysisList[[i]]) == "ictpdAnalysis")
  }
  write(rjson::toJSON(ictpdAnalysisList), file)
}

#' Load a list of ictpdAnalysis from file
#'
#' @description
#' Load a list of objects of type \code{ictpdAnalysis} from file. The file is in JSON format.
#'
#' @param file   The name of the file
#'
#' @return
#' A list of objects of type \code{ictpdAnalysis}.
#'
#' @export
loadIctpdAnalysisList <- function(file) {
  ictpdAnalysisList <- rjson::fromJSON(file = file)
  for (i in 1:length(ictpdAnalysisList)) {
    class(ictpdAnalysisList[[i]]) <- "ictpdAnalysis"
    for (j in 1:length(ictpdAnalysisList[[i]])) {
      if (is.list(ictpdAnalysisList[[i]][[j]])) {
        class(ictpdAnalysisList[[i]][[j]]) <- "args"
      }
    }
  }
  return(ictpdAnalysisList)
}

#' Create exposure-outcome combinations.
#'
#' @details
#' Create a hypothesis of interest, to be used with the \code{\link{runIctpdAnalyses}} function.
#'
#' @param exposureId   A concept ID indentifying the drug of interest in the exposure table. If
#'                     multiple strategies for picking the exposure will be tested in the analysis, a
#'                     named list of numbers can be provided instead. In the analysis, the name of the
#'                     number to be used can be specified using the \code{exposureType} parameter in
#'                     the \code{\link{createIctpdAnalysis}} function.
#' @param outcomeId    A concept ID indentifying the outcome of interest in the outcome table. If
#'                     multiple strategies for picking the outcome will be tested in the analysis, a
#'                     named list of numbers can be provided instead. In the analysis, the name of the
#'                     number to be used can be specified using the \code{outcomeType} parameter in the
#'                     \code{\link{createIctpdAnalysis}} function.
#'
#' @export
createExposureOutcome <- function(exposureId, outcomeId) {
  exposureOutcome <- OhdsiRTools::convertArgsToList(match.call(), "exposureOutcome")
  return(exposureOutcome)
}

#' Save a list of exposureOutcome to file
#'
#' @description
#' Write a list of objects of type \code{exposureOutcome} to file. The file is in JSON format.
#'
#' @param exposureOutcomeList   The exposureOutcome list to be written to file
#' @param file                  The name of the file where the results will be written
#'
#' @export
saveExposureOutcomeList <- function(exposureOutcomeList, file) {
  stopifnot(is.list(exposureOutcomeList))
  stopifnot(length(exposureOutcomeList) > 0)
  for (i in 1:length(exposureOutcomeList)) {
    stopifnot(class(exposureOutcomeList[[i]]) == "exposureOutcome")
  }
  write(rjson::toJSON(exposureOutcomeList), file)
}

#' Load a list of exposureOutcome from file
#'
#' @description
#' Load a list of objects of type \code{exposureOutcome} from file. The file is in JSON format.
#'
#' @param file   The name of the file
#'
#' @return
#' A list of objects of type \code{exposureOutcome}.
#'
#' @export
loadExposureOutcomeList <- function(file) {
  exposureOutcomeList <- rjson::fromJSON(file = file)
  for (i in 1:length(exposureOutcomeList)) {
    class(exposureOutcomeList[[i]]) <- "exposureOutcome"
  }
  return(exposureOutcomeList)
}

