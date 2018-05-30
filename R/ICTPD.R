# @file ICTPD.R
#
# Copyright 2018 Observational Health Data Sciences and Informatics
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
#
# @author Uppsala Monitoring Centre
# @author Tomas Bergvall

#' ICTemporalPatternDiscovery
#'
#' @docType package
#' @name ICTemporalPatternDiscovery
#' @import DatabaseConnector
#' @importFrom stats aggregate printCoefmat qgamma qnorm
NULL

#' @title
#' Get ICTPD counts from database
#'
#' @description
#' This function is used to load the counts needed to compute the ICTPD from a database in OMOP CDM
#' format.
#'
#' @param connectionDetails                 An R object of type \code{ConnectionDetails} created using
#'                                          the function \code{createConnectionDetails} in the
#'                                          \code{DatabaseConnector} package.
#' @param cdmDatabaseSchema                 Name of database schema that contains OMOP CDM and
#'                                          vocabulary.
#' @param oracleTempSchema                  For Oracle only: the name of the database schema where you
#'                                          want all temporary tables to be managed. Requires
#'                                          create/insert permissions to this database.
#' @param cdmVersion                        Define the OMOP CDM version used: currently supports "5".
#' @param exposureOutcomePairs              A data frame with at least two columns:
#'                                          \itemize{
#'                                            \item {"exposureId" containing the drug_concept_ID or
#'                                                  cohort_concept_id of the exposure variable}
#'                                            \item {"outcomeId" containing the condition_concept_ID or
#'                                                  cohort_concept_id of the outcome variable}
#'                                          }
#'
#' @param exposureDatabaseSchema            The name of the database schema that is the location where
#'                                          the exposure data is available.  If exposureTable =
#'                                          DRUG_ERA, exposureSchema is not used by assumed to be
#'                                          cdmSchema.  Requires read permissions to this database.
#' @param exposureTable                     The tablename that contains the exposure cohorts.  If
#'                                          exposureTable <> DRUG_ERA, then expectation is
#'                                          exposureTable has format of COHORT table:
#'                                          COHORT_DEFINITION_ID, SUBJECT_ID, COHORT_START_DATE,
#'                                          COHORT_END_DATE.
#' @param outcomeDatabaseSchema             The name of the database schema that is the location where
#'                                          the data used to define the outcome cohorts is available.
#'                                          If exposureTable = CONDITION_ERA, exposureSchema is not
#'                                          used by assumed to be cdmSchema.  Requires read permissions
#'                                          to this database.
#' @param outcomeTable                      The tablename that contains the outcome cohorts.  If
#'                                          outcomeTable <> CONDITION_OCCURRENCE, then expectation is
#'                                          outcomeTable has format of COHORT table:
#'                                          COHORT_DEFINITION_ID, SUBJECT_ID, COHORT_START_DATE,
#'                                          COHORT_END_DATE.
#' @param riskPeriodStart                   start of the risk period - can be set between 0 and 99999,
#'                                          default is 1.
#' @param riskPeriodEnd                     end of the risk period - can be set between 0 and 99999,
#'                                          default is 30.
#' @param controlPeriodStart                start of the control period - can be set between -99999 and
#'                                          0, default is -1080.
#' @param controlPeriodEnd                  end of the control period - can be set between -99999 and
#'                                          0, default is -361.
#' @param censor                            a flag indicating wether the method should censor the
#'                                          observation period at the end of exposure or not. Available
#'                                          input is 0 or 1 with default = 0.
#'
#' @return
#' An object of type \code{ictpdData} containing counts that can be used in the
#' \code{\link{calculateStatisticsIc}} function.
#'
#' @template Example
#'
#' @export
getDbIctpdData <- function(connectionDetails,
                           cdmDatabaseSchema,
                           oracleTempSchema = cdmDatabaseSchema,
                           cdmVersion = "5",
                           exposureOutcomePairs,
                           exposureDatabaseSchema = cdmDatabaseSchema,
                           exposureTable = "drug_era",
                           outcomeDatabaseSchema = cdmDatabaseSchema,
                           outcomeTable = "condition_era",
                           controlPeriodStart = -1080,
                           controlPeriodEnd = -361,
                           riskPeriodStart = 1,
                           riskPeriodEnd = 30,
                           censor = FALSE) {
    start <- Sys.time()
    exposureTable <- tolower(exposureTable)
    outcomeTable <- tolower(outcomeTable)
    if (exposureTable == "drug_era") {
        exposureStartDate <- "drug_era_start_date"
        exposureEndDate <- "drug_era_end_date"
        exposureConceptId <- "drug_concept_id"
        exposurePersonId <- "person_id"
    } else if (exposureTable == "drug_exposure") {
        exposureStartDate <- "drug_exposure_start_date"
        exposureEndDate <- "drug_exposure_end_date"
        exposureConceptId <- "drug_concept_id"
        exposurePersonId <- "person_id"
    } else {
        exposureStartDate <- "cohort_start_date"
        exposureEndDate <- "cohort_end_date"
        exposureConceptId <- "cohort_definition_id"
        exposurePersonId <- "subject_id"
    }

    if (outcomeTable == "condition_era") {
        outcomeStartDate <- "condition_era_start_date"
        outcomeEndDate <- "condition_era_end_date"
        outcomeConceptId <- "condition_concept_id"
        outcomePersonId <- "person_id"
    } else if (outcomeTable == "condition_occurrence") {
        outcomeStartDate <- "condition_start_date"
        outcomeEndDate <- "condition_end_date"
        outcomeConceptId <- "condition_concept_id"
        outcomePersonId <- "person_id"
    } else {
        outcomeStartDate <- "cohort_start_date"
        outcomeEndDate <- "cohort_end_date"
        outcomeConceptId <- "cohort_definition_id"
        outcomePersonId <- "subject_id"
    }

    # Check if connection already open:
    if (is.null(connectionDetails$conn)) {
        conn <- DatabaseConnector::connect(connectionDetails)
        on.exit(DatabaseConnector::disconnect(conn))
    } else {
        conn <- connectionDetails$conn
    }

    exposures <- data.frame(type = 1, id = unique(exposureOutcomePairs$exposureId))
    outcomes <- data.frame(type = 2, id = unique(exposureOutcomePairs$outcomeId))
    conceptsOfInterest <- rbind(exposures, outcomes)
    DatabaseConnector::insertTable(conn,
                                   "#concepts_of_interest",
                                   conceptsOfInterest,
                                   tempTable = TRUE,
                                   dropTableIfExists = TRUE)
    exposureOutcome <- data.frame(exposure_concept_id = exposureOutcomePairs$exposureId,
                                  outcome_concept_id = exposureOutcomePairs$outcomeId)
    DatabaseConnector::insertTable(conn,
                                   "#exposure_outcome",
                                   exposureOutcome,
                                   tempTable = TRUE,
                                   dropTableIfExists = TRUE)

    sql <- c()
    renderedSql <- SqlRender::loadRenderTranslateSql(sqlFilename = "IctpdParameterizedSQL.sql",
                                                     packageName = "IcTemporalPatternDiscovery",
                                                     dbms = connectionDetails$dbms,
                                                     oracleTempSchema = oracleTempSchema,
                                                     cdmDatabaseSchema = cdmDatabaseSchema,
                                                     riskPeriodStart = riskPeriodStart,
                                                     riskPeriodEnd = riskPeriodEnd,
                                                     controlPeriodStart = controlPeriodStart,
                                                     controlPeriodEnd = controlPeriodEnd,
                                                     exposureDatabaseSchema = exposureDatabaseSchema,
                                                     exposureTable = exposureTable,
                                                     exposureStartDate = exposureStartDate,
                                                     exposureEndDate = exposureEndDate,
                                                     exposureConceptId = exposureConceptId,
                                                     exposurePersonId = exposurePersonId,
                                                     outcomeDatabaseSchema = outcomeDatabaseSchema,
                                                     outcomeTable = outcomeTable,
                                                     outcomeStartDate = outcomeStartDate,
                                                     outcomeEndDate = outcomeEndDate,
                                                     outcomeConceptId = outcomeConceptId,
                                                     outcomePersonId = outcomePersonId,
                                                     censor = censor)

    writeLines(paste("Computing counts. This could take a while", sep = ""))
    DatabaseConnector::executeSql(conn, renderedSql)
    sql <- c(sql, renderedSql)

    renderedSql <- SqlRender::loadRenderTranslateSql(sqlFilename = "GetStatisticsData.sql",
                                                     packageName = "IcTemporalPatternDiscovery",
                                                     dbms = connectionDetails$dbms,
                                                     oracleTempSchema = oracleTempSchema,
                                                     exposureConceptId = exposureConceptId,
                                                     outcomeConceptId = outcomeConceptId)
    writeLines("Retrieving counts from server")
    counts <- DatabaseConnector::querySql(conn, renderedSql)
    names(counts) <- toupper(names(counts))
    sql <- c(sql, renderedSql)

    # Drop tables used in computation:
    renderedSql <- SqlRender::loadRenderTranslateSql(sqlFilename = "DropIctpdTables.sql",
                                                     packageName = "IcTemporalPatternDiscovery",
                                                     dbms = connectionDetails$dbms)
    DatabaseConnector::executeSql(conn, renderedSql, progressBar = FALSE, reportOverallTime = FALSE)
    sql <- c(sql, renderedSql)

    metaData <- list(sql = sql, exposureOutcomePairs = exposureOutcomePairs, call = match.call())
    result <- list(counts = counts, metaData = metaData)
    class(result) <- "ictpdData"
    delta <- Sys.time() - start
    writeLines(paste("Loading took", signif(delta, 3), attr(delta, "units")))
    return(result)
}

ic <- function(obs, exp, shape.add = 0.5, rate.add = 0.5, percentile = 0.025) {
    ic <- log2((obs + shape.add)/(exp + rate.add))
    ic_low <- log2(qgamma(p = percentile, shape = (obs + shape.add), rate = (exp + rate.add)))
    ic_high <- log2(qgamma(p = (1 - percentile), shape = (obs + shape.add), rate = (exp + rate.add)))
    return(list(ic = ic, ic_low = ic_low, ic_high = ic_high))
}

#' @title
#' compute the IC statistics
#'
#' @param ictpdData                An object containing the counts, as created using the
#'                                 \code{\link{getDbIctpdData}} function.
#' @param shrinkage                Shrinkage used in IRR calculations, required >0 to deal with 0 case
#'                                 counts, but larger number means more shrinkage. default is 0.5
#' @param icPercentile             The lower bound of the credibility interval for the IC values
#'                                 (IClow). default is 0.025,
#' @param metric                   Defines wether the output will contain the point estimate or the
#'                                 lower bound. Available input is 'IC and 'IC025' default is 'IC025'
#' @param multipleControlPeriods   Defines the control periods to use where 100 means the control
#'                                 period defined by controlPeriodStart/End, 010 means the period -30
#'                                 to -1 day before prescription and 001 means the control period on
#'                                 the day of prescription
#' @param multipleRiskPeriods      Defines the risk periods to use 10000 is 1-30 days, 01000 is 1 to
#'                                 360 days, 00100 is 31 to 90 days, 00010 is 91 to 180 and 00001 is
#'                                 721 to 1080 days after prescription default is '10000'
#' @description 
#' Computes the IC statistics.
#'
#' @return
#' An object of type \code{ictpdResults} containing the results.
#'
#' @template Example
#'
#' @export
calculateStatisticsIc <- function(ictpdData,
                                  multipleControlPeriods = "110",
                                  multipleRiskPeriods = "10000",
                                  shrinkage = 0.5,
                                  icPercentile = 0.025,
                                  metric = "IC025") {
    if (toupper(metric) == "IC025" & icPercentile != 0.025) {
        icPercentile <- 0.025
        warning("Forcing icPercentile to 0.025 to be able to compute IC025")
    }

    comb <- ictpdData$counts
    if (nrow(comb) == 0) {
        warning("No data found")
        result <- list(results = data.frame(), metaData = ictpdData$metaData, metric = metric)
        return(result)
    }

    expectedControl <- c()

    controlPeriodItem <- strsplit(toString(multipleControlPeriods[[1]]), "")

    tmpMat <- matrix(rep(NA, length(comb[["CXY_CONTROL"]])), ncol = 1)

    if (controlPeriodItem[[1]][1] == 1) {
        tmpMat <- cbind(tmpMat,
                        comb[["CXY_CONTROL"]]/(comb[["CX_CONTROL"]] * (comb[["CY_CONTROL"]]/comb[["C_CONTROL"]])))
    }
    if (controlPeriodItem[[1]][2] == 1) {
        tmpMat <- cbind(tmpMat, comb[["CXY_1M"]]/(comb[["CX_1M"]] * (comb[["CY_1M"]]/comb[["C_1M"]])))
    }
    if (controlPeriodItem[[1]][3] == 1) {
        tmpMat <- cbind(tmpMat, comb[["CXY_0M"]]/(comb[["CX_0M"]] * (comb[["CY_0M"]]/comb[["C_0M"]])))
    }
    if (ncol(tmpMat) == 1) {
        tmpMat <- matrix(rep(1, length(comb[["CXY_CONTROL"]])), ncol = 1)
    }
    tmpMat[is.infinite(tmpMat)] <- NA
    tmpMat[is.nan(tmpMat)] <- NA

    expectedControl <- apply(tmpMat, 1, function(x) max(x[!is.na(x)]))

    # Max of a row with only NA equals -Inf, therefore changed back to NA
    expectedControl[expectedControl == -Inf] <- NA

    # ---------------------------------- -- Calculate IC for all -- -- observation windows and -- --
    # select the largest IC025 -- ----------------------------------
    obsPeriodItem <- strsplit(toString(multipleRiskPeriods[[1]]), "")

    tmpMat <- matrix(rep(-99999999, length(comb[["CXY_CONTROL"]])), ncol = 1)

    comb["IC"] <- c()
    comb["IC_low"] <- c()
    comb["IC_high"] <- c()
    comb["CXY"] <- c()
    comb["CX"] <- c()
    comb["CY"] <- c()
    comb["C"] <- c()
    comb["expected"] <- c()

    for (cc in 1:length(comb[["CXY_CONTROL"]])) {
        maxIC_low <- -999999999
        maxIC <- NA
        maxIC_high <- NA
        CXY <- NA
        CX <- NA
        CY <- NA
        C <- NA
        expected <- NA

        if (obsPeriodItem[[1]][1] == 1) {
            tmpIC <- ic(comb[["CXY_OBSERVED_1_30"]][cc]  # Observed
                        , (comb[["CX_OBSERVED_1_30"]][cc] *
                               (comb[["CY_OBSERVED_1_30"]][cc]/comb[["C_OBSERVED_1_30"]][cc]))  # Expected_observed
                        *
                            expectedControl[cc]  # Expected_control
                        , as.numeric(shrinkage), as.numeric(shrinkage)  # Shrinkage factors
                        , as.numeric(icPercentile))

            if (!is.na(tmpIC$ic_low)) {
                if (maxIC_low < tmpIC$ic_low) {
                    maxIC <- tmpIC$ic
                    maxIC_low <- tmpIC$ic_low
                    maxIC_high <- tmpIC$ic_high
                    CXY <- comb[["CXY_OBSERVED_1_30"]][cc]
                    CX <- comb[["CX_OBSERVED_1_30"]][cc]
                    CY <- comb[["CY_OBSERVED_1_30"]][cc]
                    C <- comb[["C_OBSERVED_1_30"]][cc]
                    expected <- (comb[["CX_OBSERVED_1_30"]][cc] * (comb[["CY_OBSERVED_1_30"]][cc]/comb[["C_OBSERVED_1_30"]][cc])) *
                        expectedControl[cc]
                }
            }
        }

        if (obsPeriodItem[[1]][2] == 1) {
            tmpIC <- ic(comb[["CXY_OBSERVED_1_360"]][cc]  # Observed
                        , (comb[["CX_OBSERVED_1_360"]][cc] *
                               (comb[["CY_OBSERVED_1_360"]][cc]/comb[["C_OBSERVED_1_360"]][cc]))  # Expected_observed
                        * expectedControl[cc]  # Expected_control
                        , as.numeric(shrinkage), as.numeric(shrinkage)  # Shrinkage factors
                        , as.numeric(icPercentile))
            if (!is.na(tmpIC$ic_low)) {
                if (maxIC_low < tmpIC$ic_low) {
                    maxIC <- tmpIC$ic
                    maxIC_low <- tmpIC$ic_low
                    maxIC_high <- tmpIC$ic_high
                    CXY <- comb[["CXY_OBSERVED_1_360"]][cc]
                    CX <- comb[["CX_OBSERVED_1_360"]][cc]
                    CY <- comb[["CY_OBSERVED_1_360"]][cc]
                    C <- comb[["C_OBSERVED_1_360"]][cc]
                    expected <- (comb[["CX_OBSERVED_1_360"]][cc] * (comb[["CY_OBSERVED_1_360"]][cc]/comb[["C_OBSERVED_1_360"]][cc])) *
                        expectedControl[cc]
                }
            }
        }
        if (obsPeriodItem[[1]][3] == 1) {
            tmpIC <- ic(comb[["CXY_OBSERVED_31_90"]][cc]  # Observed
                        , (comb[["CX_OBSERVED_31_90"]][cc] *
                               (comb[["CY_OBSERVED_31_90"]][cc]/comb[["C_OBSERVED_31_90"]][cc]))  # Expected_observed
                        * expectedControl[cc]  # Expected_control
                        , as.numeric(shrinkage), as.numeric(shrinkage)  # Shrinkage factors
                        , as.numeric(icPercentile))
            if (!is.na(tmpIC$ic_low)) {
                if (maxIC_low < tmpIC$ic_low) {
                    maxIC <- tmpIC$ic
                    maxIC_low <- tmpIC$ic_low
                    maxIC_high <- tmpIC$ic_high
                    CXY <- comb[["CXY_OBSERVED_31_90"]][cc]
                    CX <- comb[["CX_OBSERVED_31_90"]][cc]
                    CY <- comb[["CY_OBSERVED_31_90"]][cc]
                    C <- comb[["C_OBSERVED_31_90"]][cc]
                    expected <- (comb[["CX_OBSERVED_31_90"]][cc] * (comb[["CY_OBSERVED_31_90"]][cc]/comb[["C_OBSERVED_31_90"]][cc])) *
                        expectedControl[cc]
                }
            }
        }
        if (obsPeriodItem[[1]][4] == 1) {
            tmpIC <- ic(comb[["CXY_OBSERVED_91_180"]][cc]  # Observed
                        , (comb[["CX_OBSERVED_91_180"]][cc] *
                               (comb[["CY_OBSERVED_91_180"]][cc]/comb[["C_OBSERVED_91_180"]][cc]))  # Expected_observed
                        * expectedControl[cc]  # Expected_control
                        , as.numeric(shrinkage), as.numeric(shrinkage)  # Shrinkage factors
                        , as.numeric(icPercentile))
            if (!is.na(tmpIC$ic_low)) {
                if (maxIC_low < tmpIC$ic_low) {
                    maxIC <- tmpIC$ic
                    maxIC_low <- tmpIC$ic_low
                    maxIC_high <- tmpIC$ic_high
                    CXY <- comb[["CXY_OBSERVED_91_180"]][cc]
                    CX <- comb[["CX_OBSERVED_91_180"]][cc]
                    CY <- comb[["CY_OBSERVED_91_180"]][cc]
                    C <- comb[["C_OBSERVED_91_180"]][cc]
                    expected <- (comb[["CX_OBSERVED_91_180"]][cc] * (comb[["CY_OBSERVED_91_180"]][cc]/comb[["C_OBSERVED_91_180"]][cc])) *
                        expectedControl[cc]
                }
            }
        }
        if (obsPeriodItem[[1]][5] == 1) {
            tmpIC <- ic(comb[["CXY_OBSERVED_721_1080"]][cc]  # Observed
                        , (comb[["CX_OBSERVED_721_1080"]][cc] *
                               (comb[["CY_OBSERVED_721_1080"]][cc]/comb[["C_OBSERVED_721_1080"]][cc]))  # Expected_observed
                        * expectedControl[cc]  # Expected_control
                        , as.numeric(shrinkage), as.numeric(shrinkage)  # Shrinkage factors
                        , as.numeric(icPercentile))
            if (!is.na(tmpIC$ic_low)) {
                if (maxIC_low < tmpIC$ic_low) {
                    maxIC <- tmpIC$ic
                    maxIC_low <- tmpIC$ic_low
                    maxIC_high <- tmpIC$ic_high
                    CXY <- comb[["CXY_OBSERVED_721_1080"]][cc]
                    CX <- comb[["CX_OBSERVED_721_1080"]][cc]
                    CY <- comb[["CY_OBSERVED_721_1080"]][cc]
                    C <- comb[["C_OBSERVED_721_1080"]][cc]
                    expected <- (comb[["CX_OBSERVED_721_1080"]][cc] * (comb[["CY_OBSERVED_721_1080"]][cc]/comb[["C_OBSERVED_721_1080"]][cc])) *
                        expectedControl[cc]
                }
            }
        }

        # ----------------------------- -- Store max values -- -----------------------------
        if (is.na(maxIC)) {
            comb[["IC"]][cc] <- NA
            comb[["IC_low"]][cc] <- NA
            comb[["IC_high"]][cc] <- NA
            comb[["CXY"]][cc] <- NA
            comb[["CX"]][cc] <- NA
            comb[["CY"]][cc] <- NA
            comb[["C"]][cc] <- NA
            comb[["expected"]][cc] <- NA
        } else {
            comb[["IC"]][cc] <- maxIC
            comb[["IC_low"]][cc] <- maxIC_low
            comb[["IC_high"]][cc] <- maxIC_high
            comb[["CXY"]][cc] <- CXY
            comb[["CX"]][cc] <- CX
            comb[["CY"]][cc] <- CY
            comb[["C"]][cc] <- C
            comb[["expected"]][cc] <- expected
        }
    }
    # Add some standard metrics that are computed by all OHDSI methods (not very meaningful for ICTPD):
    comb$LOG_RR <- comb$IC
    comb$SE_LOG_RR <- (comb$IC_high - comb$IC)/qnorm(1 - icPercentile)
    if (toupper(metric) == "IC") {
        comb$estimate <- comb$IC
    } else {
        comb$estimate <- comb$IC_low
    }
    colnames(comb) <- SqlRender::snakeCaseToCamelCase(colnames(comb))
    ictpdData$metaData$call <- list(ictpdData$metaData$call, match.call())
    result <- list(results = comb, metaData = ictpdData$metaData, metric = metric)
    class(result) <- "ictpdResults"
    return(result)
}

#' @export
print.ictpdResults <- function(x, ...) {
    output <- subset(x$results, select = c(EXPOSUREOFINTEREST, OUTCOMEOFINTEREST, estimate))
    colnames(output) <- c("Exposure concept ID", "Outcome concept ID", x$metric)
    printCoefmat(output)
}

#' @export
summary.ictpdResults <- function(object, ...) {
    object$results
}


