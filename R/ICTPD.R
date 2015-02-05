# @file ICTPD.R
#
# Copyright 2014 Observational Health Data Sciences and Informatics
#
# This file is part of ICTemporalPatternDiscovery
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


#' @title IC temporal pattern discovery
#'
#' @description
#' \code{Ictpd} generates population-level estimation from OMOP CDMv4 instance by combining a self-controlled design with a cohort design.
#'
#' @details
#' Population-level estimation method that estimates risk by combining a self-controlled and cohort design.
#'  
#' @references
#' Noren GN, Bate A, Hopstadius J, Star K, Edwards IR.  Temporal Pattern Discovery for Trends and Transient Effect: Its Application to Patient Reocrds. by combining a self-controlled design with a cohort design
#' In: Proceedings of the fourteenth ACM SIGKDD International Conference on Knowledge Discovery and Data Mining (KDD'08).ACM Press, New York, pp 963-971
#'  
#' @template StudyParameters 
#' 
#' @param analysisId  A unique identifier that can later be used to identify the results of this analysis.
#' @param storeResultsInDatabase Should the results be stored in the database?
#' @param createOutputTables  Should the output tables be created? If not, they are assumed to exist and data will be appended.
#' If the value is true and the tables exist they will be overwritten.
#' @param outputTablePrefix   Prefix used for the result tables in the \code{resultsDatabaseSchema}. 
#' 
#' @template GetDataParameters
#' 
#' @template IcCalculationParameters
#'
#' @return An object of type \code{ictpdResults} containing the results.
#' 
#' @examples \dontrun{
#' connectionDetails <- createConnectionDetails(dbms="sql server", server="server_ip")
#' ictpdResults <- runIctpd(connectionDetails, cdmDatabaseSchema, resultsDatabaseSchema, exposureOutcomePairs)
#' ictpdResults
#' }
#' @export
runIctpd <- function (connectionDetails, 
                      cdmDatabaseSchema, 
                      resultsDatabaseSchema, 
                      exposureOutcomePairs,
                      exposureDatabaseSchema = cdmDatabaseSchema,
                      exposureTable = "drug_era",
                      outcomeDatabaseSchema = cdmDatabaseSchema,
                      outcomeTable = "condition_era",
                      drugTypeConceptIdList = c(),
                      conditionTypeConceptIdList = c(),
                      
                      analysisId = 1,
                      storeResultsInDatabase = FALSE,
                      createOutputTables = TRUE,
                      outputTablePrefix = "ictpd",
                      
                      controlPeriodStart = -1080,
                      controlPeriodEnd = -361,
                      riskPeriodStart = 1,
                      riskPeriodEnd = 30,
                      censor = FALSE,
                      
                      multipleControlPeriods = '110',
                      multipleRiskPeriods = '10000',
                      shrinkage = 0.5, 
                      icPercentile = 0.025,
                      metric = "IC025"){
  cdmDatabase <- strsplit(cdmDatabaseSchema ,"\\.")[[1]][1]
  resultsDatabase <- strsplit(resultsDatabaseSchema ,"\\.")[[1]][1]
  
  conn <- connect(connectionDetails)
  connectionDetails$conn <- conn # Store it so we don't have to open it again
  
  sql <- c()
  
  if (storeResultsInDatabase){
    writeLines("Storing parameters")
    renderedSql <- loadRenderTranslateSql(sqlFilename = "StoreParameters.sql",
                                          packageName = "IcTemporalPatternDiscovery",
                                          dbms = connectionDetails$dbms,
                                          resultsDatabase = resultsDatabase,
                                          createOutputTables = createOutputTables,
                                          outputTablePrefix = outputTablePrefix,
                                          analysisId  = analysisId,
                                          controlPeriodStart = controlPeriodStart,
                                          controlPeriodEnd = controlPeriodEnd,
                                          riskPeriodStart = riskPeriodStart,
                                          riskPeriodEnd = riskPeriodEnd,
                                          censor = censor,
                                          multipleControlPeriods = multipleControlPeriods,
                                          multipleRiskPeriods = multipleRiskPeriods,
                                          shrinkage = shrinkage, 
                                          icPercentile = icPercentile,
                                          metric = metric)
    sql <- c(sql,renderedSql)
    executeSql(conn,renderedSql, progressBar = FALSE, reportOverallTime = FALSE)
  }
  
  ictpdData <- getDbIctpdData(connectionDetails = connectionDetails, 
                              cdmDatabaseSchema = cdmDatabaseSchema, 
                              resultsDatabaseSchema = resultsDatabaseSchema, 
                              exposureOutcomePairs = exposureOutcomePairs,
                              exposureDatabaseSchema = exposureDatabaseSchema,
                              exposureTable = exposureTable,
                              outcomeDatabaseSchema = outcomeDatabaseSchema,
                              outcomeTable = outcomeTable,
                              drugTypeConceptIdList = drugTypeConceptIdList,
                              conditionTypeConceptIdList = conditionTypeConceptIdList,
                              
                              controlPeriodStart = controlPeriodStart,
                              controlPeriodEnd = controlPeriodEnd,
                              riskPeriodStart = riskPeriodStart,
                              riskPeriodEnd = riskPeriodEnd,
                              censor = censor)  
  
  ictpdResults <- calculateStatisticsIC(ictpdData = ictpdData, 
                                        multipleControlPeriods = multipleControlPeriods,
                                        multipleRiskPeriods = multipleRiskPeriods,
                                        shrinkage = shrinkage, 
                                        icPercentile = icPercentile,
                                        metric = metric)
  
  sql <- c(sql,ictpdResults$metaData$sql) 
  
  if (storeResultsInDatabase){
    tableName <- paste(resultsDatabaseSchema,".",outputTablePrefix,"_results",sep="")
    resultsTable <- ictpdResults$results
    resultsTable$analysisId <- analysisId
    if (!createOutputTables){
      delSql <- "DELETE * FROM @resultsTable WHERE analysisId = @analysisId"
      delSql <- SqlRender::renderSql(delSql, resultsTable = resultsTable, analysisId = analysisId)$sql
      delSql <- SqlRender::translateSql(delSql, targetDialect = connectionDetails$dbms)$sql
      executeSql(delSql, progressBar = FALSE, reportOverallTime = FALSE)
      sql <- c(sql, delSql) 
    }
    ictpdResults$metaData$sql <- sql
    writeLines("Storing analysis results")
    DatabaseConnector::dbInsertTable(conn, tableName, resultsTable, dropTableIfExists = createOutputTables, createTable = createOutputTables)
  }
  RJDBC::dbDisconnect(conn)
  connectionDetails$conn <- NULL
  return(ictpdResults)
}

#' @title Get ICTPD counts from database
#' 
#' @description This function is used to load the counts needed to compute the ICTPD from a database in OMOP CDM format.
#' 
#' @template StudyParameters 
#' 
#' @template GetDataParameters
#' 
#' @return An object of type \code{ictpdData} containing counts that can be used in the \code{\link{calculateStatisticsIC}} function.
#' 
#' @template Example
#' 
#' @export
getDbIctpdData <- function (connectionDetails, 
                            cdmDatabaseSchema, 
                            resultsDatabaseSchema, 
                            exposureOutcomePairs,
                            exposureDatabaseSchema = cdmDatabaseSchema,
                            exposureTable = "drug_era",
                            outcomeDatabaseSchema = cdmDatabaseSchema,
                            outcomeTable = "condition_era",
                            drugTypeConceptIdList = c(),
                            conditionTypeConceptIdList = c(),
                            
                            controlPeriodStart = -1080,
                            controlPeriodEnd = -361,
                            riskPeriodStart = 1,
                            riskPeriodEnd = 30,
                            censor = FALSE) {
  exposureTable <- tolower(exposureTable)
  outcomeTable <- tolower(outcomeTable)
  if (exposureTable == "drug_era"){
    exposureStartDate = "drug_era_start_date"
    exposureEndDate = "drug_era_end_date"
    exposureConceptId = "drug_concept_id"
    exposurePersonId =  "person_id"
  } else if (exposureTable == "drug_exposure"){
    exposureStartDate = "drug_exposure_start_date"
    exposureEndDate = "drug_exposure_end_date"
    exposureConceptId = "drug_concept_id"
    exposurePersonId =  "person_id"
  } else {
    exposureStartDate = "cohort_start_date"
    exposureEndDate = "cohort_end_date"
    exposureConceptId = "cohort_concept_id"
    exposurePersonId =  "subject_id"
  }
  
  if (outcomeTable == "condition_era"){
    outcomeStartDate = "condition_era_start_date";
    outcomeEndDate = "condition_era_end_date"
    outcomeConceptId = "condition_concept_id"
    outcomePersonId =  "person_id"
  } else if (outcomeTable == "condition_occurrence"){
    outcomeStartDate = "condition_start_date"
    outcomeEndDate = "condition_end_date"
    outcomeConceptId = "condition_concept_id"
    outcomePersonId =  "person_id"
  } else {
    outcomeStartDate = "cohort_start_date"
    outcomeEndDate = "cohort_end_date"
    outcomeConceptId = "cohort_concept_id"
    outcomePersonId =  "subject_id"
  }
  
  cdmDatabase <- strsplit(cdmDatabaseSchema ,"\\.")[[1]][1]
  resultsDatabase <- strsplit(resultsDatabaseSchema ,"\\.")[[1]][1]
  
  conceptsOfInterestTable <- paste(resultsDatabaseSchema,"concepts_of_interest",sep=".")
  exposureOutcomeTable <- paste(resultsDatabaseSchema,"exposure_outcome",sep=".")
  
  #Check if connection already open:
  if (is.null(connectionDetails$conn)){
    conn <- connect(connectionDetails)
  } else {
    conn <- connectionDetails$conn
  }
  
  writeLines("Writing exposures and outcomes to database")
  exposures <- data.frame(type = 1, id = unique(exposureOutcomePairs$exposureConceptId))
  DatabaseConnector::dbInsertTable(conn, conceptsOfInterestTable, exposures, dropTableIfExists = TRUE)
  
  outcomes <- data.frame(type = 2, id = unique(exposureOutcomePairs$outcomeConceptId))
  DatabaseConnector::dbInsertTable(conn, conceptsOfInterestTable, outcomes, dropTableIfExists = FALSE, createTable = FALSE)
  
  exposureOutcome <- data.frame(exposure_concept_id = exposureOutcomePairs$exposureConceptId, outcome_concept_id = exposureOutcomePairs$outcomeConceptId)
  DatabaseConnector::dbInsertTable(conn, exposureOutcomeTable, exposureOutcome, dropTableIfExists = TRUE)
  
  sql <- c()
  renderedSql <- SqlRender::loadRenderTranslateSql(sqlFilename = "IctpdParameterizedSQL.sql",
                                                   packageName = "IcTemporalPatternDiscovery",
                                                   dbms = connectionDetails$dbms,
                                                   cdmDatabaseSchema = cdmDatabaseSchema, 
                                                   resultsDatabaseSchema = resultsDatabaseSchema, 
                                                   cdmDatabase = cdmDatabase,
                                                   resultsDatabase = resultsDatabase,
                                                   drugTypeConceptIdList = drugTypeConceptIdList,
                                                   conditionTypeConceptIdList = conditionTypeConceptIdList,
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
                                                   censor = censor
  )
  
  writeLines(paste("Computing counts. This could take a while",sep=""))
  executeSql(conn, renderedSql)
  sql <- c(sql, renderedSql)
  
  renderedSql <- SqlRender::loadRenderTranslateSql(sqlFilename = "GetStatisticsData.sql",
                                                   packageName = "IcTemporalPatternDiscovery",
                                                   dbms = connectionDetails$dbms,
                                                   exposureConceptId = exposureConceptId,
                                                   exposurePersonId = exposurePersonId,
                                                   outcomeStartDate = outcomeStartDate,
                                                   outcomeEndDate = outcomeEndDate)
  writeLines("Retrieving counts from server")
  counts <- querySql(conn, renderedSql)
  sql <- c(sql, renderedSql)
  
  #Drop tables used in computation:
  renderedSql <- SqlRender::loadRenderTranslateSql(sqlFilename = "DropTables.sql",
                                                   packageName = "IcTemporalPatternDiscovery",
                                                   dbms = connectionDetails$dbms)
  executeSql(conn, renderedSql, progressBar = FALSE, reportOverallTime = FALSE)
  sql <- c(sql, renderedSql)                                                 
  
  # Close connection if it was openend in this function:
  if (is.null(connectionDetails$conn)){
    RJDBC::dbDisconnect(conn)
  } 
  
  metaData <- list(sql = sql,
                   exposureOutcomePairs = exposureOutcomePairs,
                   call = match.call()
  )
  result <- list(counts = counts,
                 metaData = metaData
  )
  class(result) <- "ictpdData"
  return(result)
}

ic <- function(obs,exp,shape.add=0.5,rate.add=0.5, percentile=0.025) {
  ic    <- log2(                     (obs+shape.add)/     (exp+rate.add) )
  ic_low <- log2(qgamma(p=percentile    , shape=(obs+shape.add), rate=(exp+rate.add)))
  ic_high <- log2(qgamma(p=(1-percentile), shape=(obs+shape.add), rate=(exp+rate.add)))
  return(list(ic=ic,ic_low=ic_low,ic_high=ic_high))
}

#' @title compute the IC statistics
#' 
#' @param ictpdData An object containing the counts, as created using the \code{\link{getDbIctpdData}} function.
#' 
#' @template IcCalculationParameters
#' 
#' @return An object of type \code{ictpdResults} containing the results.
#' 
#' @template Example
#' 
#' @export
calculateStatisticsIC <- function (ictpdData, 
                                   multipleControlPeriods = '110',
                                   multipleRiskPeriods = '10000',
                                   shrinkage = 0.5, 
                                   icPercentile = 0.025,
                                   metric = "IC025"
) {
  if (toupper(metric) == "IC025" & icPercentile != 0.025){
    icPercentile = 0.025
    warning("Forcing icPercentile to 0.025 to be able to compute IC025")
  }
  
  comb <- ictpdData$counts
  
  expectedControl = c();
  
  controlPeriodItem=strsplit(toString(multipleControlPeriods[[1]][controlPeriod]),'');
  
  tmpMat <- matrix(rep(NA, length(comb[['CXY_CONTROL']])), ncol=1);
  
  if(controlPeriodItem[[1]][1] == 1) {
    tmpMat <- cbind(tmpMat, comb[['CXY_CONTROL']] / (comb[['CX_CONTROL']] * (comb[['CY_CONTROL']]  / comb[['C_CONTROL']])));
  }
  if(controlPeriodItem[[1]][2] == 1) {
    tmpMat <- cbind(tmpMat, comb[['CXY_1M']] / (comb[['CX_1M']] * (comb[['CY_1M']]  / comb[['C_1M']])));
  }
  if(controlPeriodItem[[1]][3] == 1) {
    tmpMat <- cbind(tmpMat, comb[['CXY_0M']] / (comb[['CX_0M']] * (comb[['CY_0M']]  / comb[['C_0M']])));
  }
  if(ncol(tmpMat) == 1) {
    tmpMat <- matrix(rep(1, length(comb[['CXY_CONTROL']])), ncol=1);
  }
  tmpMat[tmpMat == -Inf] <- NA;
  tmpMat[tmpMat == Inf] <- NA;
  tmpMat[tmpMat == NaN] <- NA;
  
  expectedControl <- apply(tmpMat, 1, function(x) max(x[!is.na(x)]));
  
  # Max of a row with only NA equals -Inf, therefore changed back to NA
  expectedControl[expectedControl == -Inf] <- NA;
  
  # ----------------------------------
  # --  Calculate IC for all        --
  # --  observation windows and     --
  # --  select the largest IC025    --
  # ----------------------------------
  obsPeriodItem=strsplit(toString(multipleRiskPeriods[[1]][obsPeriod]),'');
  
  tmpMat <- matrix(rep(-99999999, length(comb[['CXY_CONTROL']])), ncol=1);
  
  comb['IC'] <- c();
  comb['IC_low'] <- c();
  comb['IC_high'] <- c();
  comb['CXY'] <- c();
  comb['CX'] <- c();
  comb['CY'] <- c();
  comb['C'] <- c();
  comb['expected'] <- c()
  
  for(cc in 1:length(comb[['CXY_CONTROL']])) {
    maxIC_low <- -999999999;
    maxIC <- NA;
    maxIC_high <- NA;
    CXY <- NA;
    CX <- NA;
    CY <- NA;
    C <- NA;
    expected <- NA;
    
    if(obsPeriodItem[[1]][1] == 1) {
      tmpIC <- ic(  comb[['CXY_OBSERVED_1_30']][cc]                                                               # Observed
                    ,(comb[['CX_OBSERVED_1_30']][cc] * (comb[['CY_OBSERVED_1_30']][cc] / comb[['C_OBSERVED_1_30']][cc]))     # Expected_observed
                    * expectedControl[cc]                                                                       	# Expected_control
                    , as.numeric(shrinkage)
                    , as.numeric(shrinkage)		                 									# Shrinkage factors
                    , as.numeric(icPercentile));
      
      if( ! is.na(tmpIC$ic_low)) {
        if(maxIC_low < tmpIC$ic_low) {
          maxIC      <- tmpIC$ic;
          maxIC_low  <- tmpIC$ic_low;
          maxIC_high <- tmpIC$ic_high;
          CXY        <- comb[['CXY_OBSERVED_1_30']][cc];
          CX         <- comb[['CX_OBSERVED_1_30']][cc];
          CY         <- comb[['CY_OBSERVED_1_30']][cc];
          C          <- comb[['C_OBSERVED_1_30']][cc];
          expected   <- (comb[['CX_OBSERVED_1_30']][cc] * (comb[['CY_OBSERVED_1_30']][cc] / comb[['C_OBSERVED_1_30']][cc])) * expectedControl[cc];
        }
      }
    }
    
    if(obsPeriodItem[[1]][2] == 1) {
      tmpIC <- ic(  comb[['CXY_OBSERVED_1_360']][cc]                                                               # Observed
                    ,(comb[['CX_OBSERVED_1_360']][cc] * (comb[['CY_OBSERVED_1_360']][cc] / comb[['C_OBSERVED_1_360']][cc]))   	# Expected_observed
                    * expectedControl[cc]                                                                        	# Expected_control
                    , as.numeric(shrinkage)
                    , as.numeric(shrinkage)  	                 									# Shrinkage factors
                    , as.numeric(icPercentile));
      if( ! is.na(tmpIC$ic_low)) {
        if(maxIC_low < tmpIC$ic_low) {
          maxIC      <- tmpIC$ic;
          maxIC_low  <- tmpIC$ic_low;
          maxIC_high <- tmpIC$ic_high;
          CXY        <- comb[['CXY_OBSERVED_1_360']][cc];
          CX         <- comb[['CX_OBSERVED_1_360']][cc];
          CY         <- comb[['CY_OBSERVED_1_360']][cc];
          C          <- comb[['C_OBSERVED_1_360']][cc];
          expected   <- (comb[['CX_OBSERVED_1_360']][cc] * (comb[['CY_OBSERVED_1_360']][cc] / comb[['C_OBSERVED_1_360']][cc])) * expectedControl[cc];
        }
      }
    }
    if(obsPeriodItem[[1]][3] == 1) {
      tmpIC <- ic(  comb[['CXY_OBSERVED_31_90']][cc]                                                               # Observed
                    ,(comb[['CX_OBSERVED_31_90']][cc] * (comb[['CY_OBSERVED_31_90']][cc] / comb[['C_OBSERVED_31_90']][cc]))   	# Expected_observed
                    * expectedControl[cc]                                                                        	# Expected_control
                    , as.numeric(shrinkage)
                    , as.numeric(shrinkage)  	                 									# Shrinkage factors
                    , as.numeric(icPercentile));
      if( ! is.na(tmpIC$ic_low)) {
        if(maxIC_low < tmpIC$ic_low) {
          maxIC      <- tmpIC$ic;
          maxIC_low  <- tmpIC$ic_low;
          maxIC_high <- tmpIC$ic_high;
          CXY        <- comb[['CXY_OBSERVED_31_90']][cc];
          CX         <- comb[['CX_OBSERVED_31_90']][cc];
          CY         <- comb[['CY_OBSERVED_31_90']][cc];
          C          <- comb[['C_OBSERVED_31_90']][cc];
          expected   <- (comb[['CX_OBSERVED_31_90']][cc] * (comb[['CY_OBSERVED_31_90']][cc] / comb[['C_OBSERVED_31_90']][cc])) * expectedControl[cc];
        }
      }
    }
    if(obsPeriodItem[[1]][4] == 1) {
      tmpIC <- ic(  comb[['CXY_OBSERVED_91_180']][cc]                                                               # Observed
                    ,(comb[['CX_OBSERVED_91_180']][cc] * (comb[['CY_OBSERVED_91_180']][cc] / comb[['C_OBSERVED_91_180']][cc]))   	# Expected_observed
                    * expectedControl[cc]                                                                        	# Expected_control
                    , as.numeric(shrinkage)
                    , as.numeric(shrinkage)  	                 									# Shrinkage factors
                    , as.numeric(icPercentile));
      if( ! is.na(tmpIC$ic_low)) {
        if(maxIC_low < tmpIC$ic_low) {
          maxIC      <- tmpIC$ic;
          maxIC_low  <- tmpIC$ic_low;
          maxIC_high <- tmpIC$ic_high;
          CXY        <- comb[['CXY_OBSERVED_91_180']][cc];
          CX         <- comb[['CX_OBSERVED_91_180']][cc];
          CY         <- comb[['CY_OBSERVED_91_180']][cc];
          C          <- comb[['C_OBSERVED_91_180']][cc];
          expected   <- (comb[['CX_OBSERVED_91_180']][cc] * (comb[['CY_OBSERVED_91_180']][cc] / comb[['C_OBSERVED_91_180']][cc])) * expectedControl[cc];
        }
      }
    }
    if(obsPeriodItem[[1]][5] == 1) {
      tmpIC <- ic(  comb[['CXY_OBSERVED_721_1080']][cc]                                                               # Observed
                    ,(comb[['CX_OBSERVED_721_1080']][cc] * (comb[['CY_OBSERVED_721_1080']][cc] / comb[['C_OBSERVED_721_1080']][cc]))   	# Expected_observed
                    * expectedControl[cc]                                                                        	# Expected_control
                    , as.numeric(shrinkage)
                    , as.numeric(shrinkage)  	                 									# Shrinkage factors
                    , as.numeric(icPercentile));
      if( ! is.na(tmpIC$ic_low)) {
        if(maxIC_low < tmpIC$ic_low) {
          maxIC      <- tmpIC$ic;
          maxIC_low  <- tmpIC$ic_low;
          maxIC_high <- tmpIC$ic_high;
          CXY        <- comb[['CXY_OBSERVED_721_1080']][cc];
          CX         <- comb[['CX_OBSERVED_721_1080']][cc];
          CY         <- comb[['CY_OBSERVED_721_1080']][cc];
          C          <- comb[['C_OBSERVED_721_1080']][cc];
          expected   <- (comb[['CX_OBSERVED_721_1080']][cc] * (comb[['CY_OBSERVED_721_1080']][cc] / comb[['C_OBSERVED_721_1080']][cc])) * expectedControl[cc];
        }
      }
    }
    
    # -----------------------------
    # --    Store max values     --
    # -----------------------------
    if(is.na(maxIC)) {
      comb[['IC']][cc] <- NA;
      comb[['IC_low']][cc] <- NA;
      comb[['IC_high']][cc] <- NA;
      comb[['CXY']][cc] <- NA;
      comb[['CX']][cc]  <- NA;
      comb[['CY']][cc]  <- NA;
      comb[['C']][cc]   <- NA;
      comb[['expected']][cc]   <- NA;
    } else {
      comb[['IC']][cc] <- maxIC;
      comb[['IC_low']][cc] <- maxIC_low;
      comb[['IC_high']][cc] <- maxIC_high;
      comb[['CXY']][cc] <- CXY;
      comb[['CX']][cc]  <- CX;
      comb[['CY']][cc]  <- CY;
      comb[['C']][cc]   <- C;
      comb[['expected']][cc]   <- expected;
    }
  }
  # Add some standard metrics that are computed by all OHDSI methods (not very meaningful for ICTPD):
  comb$logRr <- comb$IC
  comb$seLogRr <- (comb$IC_high - comb$IC) / qnorm(1-icPercentile)
  
  if (toupper(metric) == "IC"){
    comb$estimate <- comb$IC
  } else {
    comb$estimate <- comb$IC_low
  }
  ictpdData$metaData$call <- list(ictpdData$metaData$call, match.call())
  result <- list(results = comb,
                 metaData = ictpdData$metaData,
                 metric = metric)  
  class(result) <- "ictpdResults"
  return(result) 
}

#' @export
print.ictpdResults <- function(x, ...){
  output <- subset(x$results, select = c(EXPOSUREOFINTEREST,OUTCOMEOFINTEREST,estimate))  
  colnames(output) <- c("Exposure concept ID","Outcome concept ID", x$metric)
  printCoefmat(output)
}

#' @export
summary.ictpdResults <- function(object, ...){
  object$results
}

#' @export
runIctpdAnalyses <- function(connectionDetails, 
                             cdmDatabaseSchema, 
                             resultsDatabaseSchema, 
                             exposureOutcomePairs,
                             exposureDatabaseSchema = cdmDatabaseSchema,
                             exposureTable = "drug_era",
                             outcomeDatabaseSchema = cdmDatabaseSchema,
                             outcomeTable = "condition_era",
                             drugTypeConceptIdList = c(),
                             conditionTypeConceptIdList = c(),
                             ictpdAnalysisList){
 #Todo: Need to implement this function so it efficiently runs over all analyses
}

#' @title writeIctpdAnalysesDetailsToFile
#'
#' @description
#' \code{writeIctpdAnalysesDetailsToFile} writes an object of type \code{analysesDetails} to a CSV file
#'  
#' @param ictpdAnalysesDetails    the \code{ictpdAnalysesDetails} to be written to file
#' @param file                  the name of the file where the results will be written
#' @examples \dontrun{
#'   analysesDetails <- NULL
#'   analysesDetails <- appendToIctpdAnalysesDetails(createIctpdAnalysisDetails(analysisId = 1),analysesDetails)
#'   analysesDetails <- appendToIctpdAnalysesDetails(createIctpdAnalysisDetails(analysisId = 2),analysesDetails)
#'   writeIctpdAnalysesDetailsToFile(analysesDetails,"c:/temp/test.csv")
#' }
#' @export
writeIctpdAnalysesDetailsToFile <- function(ictpdAnalysesDetails, file){
  stopifnot(class(ictpdAnalysesDetails) == "ictpdAnalysesDetails")
  
  #Convert ictpdAnalysesDetails to a data.frame, converting any nested vectors into semicolon-delimited strings:
  f <- ictpdAnalysesDetails
  d <- data.frame()
  for (row in 1:length(f)){
    class(f[[row]]) <- "list"
    for (column in 1:length(f[[row]])){
      if ((class(f[[row]][[column]]) == "numeric") && (length(f[[row]][[column]]) > 1))
        f[[row]][[column]] = paste(f[[row]][[column]],collapse=";")
    }
    d <- rbind(d,as.data.frame(f[[row]]))
  }
  
  write.csv(d,file=file, row.names=FALSE)
}


#' @title readIctpdAnalysesDetailsFromFile
#'
#' @description
#' \code{readIctpdAnalysesDetailsFromFile} reads an object of type \code{analysesDetails} from a CSV file
#'  
#' @param file                  the name of the file to be loaded
#' @return An object of type \code{analysesDetails}
#' @examples \dontrun{
#'   connectionDetails <- createConnectionDetails(dbms="sql server", server="RNDUSRDHIT07.jnj.com")
#'   analysesDetails <- readIctpdAnalysesDetailsFromFile("c:/temp/test.csv")
#'   ictpdResult <- IcTemporalPatternDiscovery(analysesDetails, connectionDetails, "cdm_truven_mdcr", "scratch", sourceName = "cdm_truven_mdcr", exposuresOfInterest = c(767410,1314924,907879), outcomesOfInterest = c(444382, 79106, 138825), outcomeTable = "condition_era")
#' }
#' @export
readIctpdAnalysesDetailsFromFile <- function(file){
  d <- read.csv(file)
  d[is.na(d)] <- ""
  ictpdAnalysesDetails <- list()
  for (row in 1:nrow(d)){
    ictpdAnalysisDetails <- as.list(d[row,])
    for (column in c("drugTypeConceptIdList","conditionTypeConceptIdList")){
      ictpdAnalysisDetails[[column]] <- as.numeric(unlist(strsplit(as.character(d[row,column]),";")))
    }
    class(ictpdAnalysisDetails) = "ictpdAnalysisDetails"
    ictpdAnalysesDetails[[length(ictpdAnalysesDetails)+1]] <- ictpdAnalysisDetails
  }
  class(ictpdAnalysesDetails) <- "ictpdAnalysesDetails"
  ictpdAnalysesDetails
}






