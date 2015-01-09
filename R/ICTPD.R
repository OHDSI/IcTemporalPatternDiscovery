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


# Loads the results from the server and adds them to a results object
addResults <- function(results,conn) {
  sql <- "SELECT * FROM @table WHERE sourceName = '@sourceName' AND analysisId IN (@analysisIds) AND exposureConceptId IN (@exposureConceptIds) AND outcomeConceptId IN (@outcomeConceptIds)"
  sql <- renderSql(sql,
                   table = results$resultsTable, 
                   sourceName = results$sourceName,
                   analysisIds = results$analysisIds, 
                   exposureConceptIds = results$exposureOutcomePairs$exposureConceptId,
                   outcomeConceptIds = results$exposureOutcomePairs$outcomeConceptId
  )$sql
  results$effectEstimates <- dbGetQuery(conn,sql)
  sql <- "SELECT * FROM @table WHERE analysisId IN (@analysisIds)" 
  sql <- renderSql(sql,
                   table = results$analysisTable, 
                   analysisIds = results$analysisIds
  )$sql
  results$analyses <- dbGetQuery(conn,sql)
  results
}

#' @title IC temporal pattern discovery
#'
#' @description
#' \code{Ictpd} generates population-level estimation from OMOP CDMv4 instance by combining a self-controlled design with a cohort design.
#'
#' @usage 
#' ictpd(connectionDetails, cdmSchema, resultsSchema, resultsTablePrefix = "ictpd", createResultsTable = TRUE, sourceName = "", exposureOutcomePairs, exposureTable = "drug_era", outcomeTable = "condition_era")
#' ictpd(connectionDetails, cdmSchema, resultsSchema, resultsTablePrefix = "ictpd", createResultsTable = TRUE, sourceName = "", exposureOutcomePairs, exposureTable = "drug_era", outcomeTable = "condition_era", analysisId = 1, drugTypeConceptIdList = c(38000182), conditionTypeConceptIdList = c(38000247))
#' ictpd(connectionDetails, cdmSchema, resultsSchema, resultsTablePrefix = "ictpd", createResultsTable = TRUE, sourceName = "", exposureOutcomePairs, exposureTable = "drug_era", outcomeTable = "condition_era", analysisId = 1, drugTypeConceptIdList = c(38000182), conditionTypeConceptIdList = c(38000247), controlPeriodStart = -1080, controlPeriodEnd = -361, multipleControlPeriods = 110, multipeObservationPeriods = '10000', shrinkage = 0.50, icPercentile = 0.025, metric = "IC025", censor = 0) 
#' 
#' @details
#' Population-level estimation method that estimates risk by combining a self-controlled and cohort design.
#'  
#' @references
#' Norén GN, Bate A, Hopstadius J, Star K, Edwards IR.  Temporal Pattern Discovery for Trends and Transient Effect: Its Application to Patient Reocrds. by combining a self-controlled design with a cohort design
#' In: Proceedings of the fourteenth ACM SIGKDD International Conference on Knowledge Discovery and Data Mining (KDD'08).ACM Press, New York, pp 963-971
#'  
#' @param connectionDetails  An R object of type \code{ConnectionDetails} created using the function \code{createConnectionDetails} in the \code{DatabaseConnector} package.
#' @param cdmSchema  		Name of database schema that contains OMOP CDM and vocabulary.
#' @param resultsSchema		Name of database schema that we can write results to.
#' @param resultsTablePrefix  Prefix used for the result tables in the \code{resultsSchema}. 
#' @param createResultsTable If true, a new empty table will be created to store the results. If false, results will be inserted into the existing table.
#' @param sourceName		Name of the database, as recorded in results.
#' @param exposureOutcomePairs  A data frame with at least two columns:
#' \itemize{
#'   \item{"exposureConceptId" containing the drug_concept_ID or cohort_concept_id of the exposure variable}
#'   \item{"outcomeConceptId" containing the condition_concept_ID or cohort_concept_id of the outcome variable}
#' }
#' @param exposureTable	\code{exposureTable="drugEra"} or \code{exposureTable="cohort"}.
#' @param outcomeTable	\code{outcomeTable="conditionEra"} or \code{outcomeTable="cohort"}.
#' @param analysisId  	A unique identifier that can later be used to identify the results of this analysis.

#' @param drugTypeConceptIdList	Which drug_type to use:  generally only use 1 value (ex:  30d era).
#' @param conditionTypeConceptIdList	Which condition_type to use:  generally only use 1 value (ex:  30d era).
#' @param shrinkage	shrinkage used in IRR calculations, required >0 to deal with 0 case counts, but larger number means more shrinkage. default is 0.5
#' @param ictpdAnalysisDetails  object specifyng a set of analysis choices.
#' @param ictpdAnalysesDetails  object specifyng one or several sets of analysis choices.
#' 
#' @param controlPeriodStart start of the control period - can be set between -99999 and 0, default is -1080
#' @param controlPeriodEnd end of the control period - can be set between -99999 and 0, default is -361
#' @param multipleControlPeriods Defines the control periods to use where 100 means the control period defined by controlPeriodStart/End, 010 means the period -30 to -1 day before prescription and 001 means the contorl period on the day of prescription
#' @param multipleObservationPeriods Defines the observation periods to use 10000 is 1-30 days, 01000 is 1 to 360 days, 00100 is 31 to 90 days, 00010 is 91 to 180 and 00001 is 721 to 1080 days after prescription default is '10000'
#' @param icPercentile the lower bound of the credibility interval for the IC values (IClow). default is 0.025,
#' @param metric defines wether the output will contain the point estimate or the lower bound. Available input is 'IC and 'IC025' default is 'IC025'
#' @param censor a flag indicating wether the method should censor the observation period at the end of exposure or not. Available input is 0 or 1 with default = 0. 

#' @return An object of type \code{ictpdResults} containing details for connecting to the database containing the results 
#' @examples \dontrun{
#' 
#' connectionDetails <- createConnectionDetails(dbms="sql server", server="server_ip", schema="omop_cdm_database")
#' exposureOutcomePairs = data.frame(outcomeConceptId = c(196794, 196794, 312648), exposureConceptId = c(1501700, 1545958, 1551803))
#' ictpdResult <- ictpd(connectionDetails, cdmSchema = "OmopCdm_Simulated", resultsSchema = "OmopCdm_Simulated", sourceName = "OmopCdm_Simulated", exposureOutcomePairs = exposureOutcomePairs)
#' 
#' }
#' @export
ictpd <- function(...){
  UseMethod("Ictpd") 
}

#' @export
Ictpd.connectionDetails <- function (connectionDetails, 
                                     cdmSchema, 
                                     resultsSchema, 
                                     sourceName = "", 
                                     exposureOutcomePairs,

                                     resultsTablePrefix = "ictpd", 
                                     createResultsTable = TRUE,
                                     exposureTable = "drug_era",
                                     outcomeTable = "condition_era",
                                     analysisId = 1,
                                     
                                     drugTypeConceptIdList = c(38000182),
                                     conditionTypeConceptIdList = c(38000247),
                                     
                                     controlPeriodStart = -1080,
                                     controlPeriodEnd = -361,
                                     customObservationPeriodStart = 1,
                                     customObservationPeriodEnd = 30, 
                                     multipleControlPeriods = '110',
                                     multipleObservationPeriods = '10000',
                                     shrinkage = 0.50,
                                     icPercentile = 0.025,
                                     metric = "IC025",
                                     censor = 0
                                     ){
  
  #Check if connection already open:
  writeLines("Check if connection already open:")
  if (is.null(connectionDetails$conn)){
    conn <- connect(connectionDetails)
  } else {
    conn <- connectionDetails$conn
  }
  
  sql <- c()

  # Store parameters
  writeLines("Store all parameters")
  renderedSql <- loadRenderTranslateSql(sqlFilename        = "StoreParameters.sql",
                                        packageName        = "IcTemporalPatternDiscovery",
                                        dbms               = connectionDetails$dbms,
                                        
                                        analysisId         = analysisId,
                                        cdmSchema          = cdmSchema, 
                                        resultsSchema      = resultsSchema, 
                                        resultsTablePrefix = resultsTablePrefix,
                                        sourceName         = sourceName, 
                                        
                                        resultsTablePrefix = resultsTablePrefix, 
                                        createResultsTable = createResultsTable,
                                        exposureTable = exposureTable,
                                        outcomeTable = outcomeTable,
                                        
                                        drugTypeConceptIdList = drugTypeConceptIdList,
                                        conditionTypeConceptIdList = conditionTypeConceptIdList,
                                        
                                        listExposureConceptId = paste(as.character(exposureOutcomePairs$exposureConceptId), collapse=", "),
                                        listOutcomeConceptId = paste(as.character(exposureOutcomePairs$outcomeConceptId), collapse=", "),
                                        
                                        controlPeriodStart = controlPeriodStart,
                                        controlPeriodEnd = controlPeriodEnd,
                                        customObservationPeriodStart = customObservationPeriodStart,
                                        customObservationPeriodEnd = customObservationPeriodEnd, 
                                        multipleControlPeriods = multipleControlPeriods,
                                        multipleObservationPeriods = multipleObservationPeriods,
                                        shrinkage = shrinkage,
                                        icPercentile = icPercentile,
                                        metric = metric,
                                        censor = censor
                                        

    )
  #writeLines(paste(renderedSql, sep=""))
  sql <- c(sql,renderedSql)
  executeSql(conn,renderedSql)
  
  # Store all exposures
  writeLines("Store all exposures")
  for (exposureConceptId in unique(exposureOutcomePairs$exposureConceptId))
  {
    renderedSql <- loadRenderTranslateSql(sqlFilename        = "PopulateConceptsOfInterest.sql",
                                          packageName        = "IcTemporalPatternDiscovery",
                                          dbms               = connectionDetails$dbms,
                                          resultsSchema      = resultsSchema, 
                                          resultsTablePrefix = resultsTablePrefix, 
                                          analysisId         = analysisId,
                                          conceptType        = 1,
                                          conceptId          = exposureConceptId
    )
    sql <- c(sql,renderedSql)
    executeSql(conn,renderedSql)
    
  }
  
  # Store all outcomes
  writeLines("Store all outcomes")
  for (outcomeConceptId in unique(exposureOutcomePairs$outcomeConceptId))
  {
    renderedSql <- loadRenderTranslateSql(sqlFilename        = "PopulateConceptsOfInterest.sql",
                                          packageName        = "IcTemporalPatternDiscovery",
                                          dbms               = connectionDetails$dbms,
                                          resultsSchema      = resultsSchema, 
                                          resultsTablePrefix = resultsTablePrefix, 
                                          analysisId         = analysisId,
                                          conceptType        = 2,
                                          conceptId          = outcomeConceptId
    )
    sql <- c(sql,renderedSql)
    executeSql(conn,renderedSql)
    
  }
  
  # Store link between exposure and outcome
  writeLines("Store link between exposure and outcome")
  for (i in 1:nrow(exposureOutcomePairs)) # exposureOutcomePair in unique(exposureOutcomePairs))
  {
    exposureOutcomePair <- exposureOutcomePairs[i,]
    
    writeLines(paste("exposureConceptId: ", exposureOutcomePair$exposureConceptId, " outcomeConceptId: ", exposureOutcomePair$outcomeConceptId, sep=""))
    
    renderedSql <- loadRenderTranslateSql(sqlFilename        = "PopulateLinkExposureOutcome.sql",
                                          packageName        = "IcTemporalPatternDiscovery",
                                          dbms               = connectionDetails$dbms,
                                          resultsSchema      = resultsSchema, 
                                          resultsTablePrefix = resultsTablePrefix, 
                                          analysisId         = analysisId,
                                          exposureConceptId  = exposureOutcomePair$exposureConceptId,
                                          outcomeConceptId   = exposureOutcomePair$outcomeConceptId
                                          )
    sql <- c(sql,renderedSql)
    executeSql(conn,renderedSql)
    
  }
  
  writeLines("Generate temporary tables")
  renderedSql <- loadRenderTranslateSql(sqlFilename = "IctpdParameterizedSQL.sql",
                                        packageName = "IcTemporalPatternDiscovery",
                                        dbms = connectionDetails$dbms,
                                        cdmSchema = cdmSchema, 
                                        resultsSchema = resultsSchema, 
                                        resultsTablePrefix = resultsTablePrefix, 
                                        createResultsTable = createResultsTable,
                                        sourceName = sourceName,
                                        analysisId = analysisId,
                                                                    
                                        drugTypeConceptIdList = drugTypeConceptIdList,
                                        conditionTypeConceptIdList = conditionTypeConceptIdList,
                                      
                                        customObservationPeriodStart = customObservationPeriodStart,
                                        customObservationPeriodEnd = customObservationPeriodEnd, 
                                        controlPeriodStart = controlPeriodStart,
                                        controlPeriodEnd = controlPeriodEnd,
                                        multipleControlPeriods = multipleControlPeriods,
                                        shrinkage = shrinkage,
                                        icPercentile = icPercentile,
                                        exposureTable = exposureTable,
                                        outcomeTable = outcomeTable                                         
                                        )
    
  writeLines(paste("Executing analysis (analysisId = ",analysisId,") This could take a while",sep=""))
  #writeLines(paste(renderedSql, sep=""))
  executeSql(conn,renderedSql)
  sql <- c(sql,renderedSql)
  
  writeLines("Get results")
  renderedSql <- loadRenderTranslateSql(sqlFilename = "GetStatisticsData.sql",
                                        packageName = "IcTemporalPatternDiscovery",
                                        dbms = connectionDetails$dbms,
                                        cdmSchema = cdmSchema, 
                                        resultsSchema = resultsSchema, 
                                        resultsTablePrefix = resultsTablePrefix, 
                                        createResultsTable = createResultsTable,
                                        sourceName = sourceName,
                                        analysisId = analysisId
                                        
  )
  
  writeLines(paste("Executing IC calculations (analysisId = ",analysisId,")",sep=""))
  #writeLines(paste(renderedSql, sep=""))
  queryResults <- querySql(conn,renderedSql)
  sql <- c(sql,renderedSql)
  
  writeLines("Calculate statistics")
  comb <- CalcStatisticsIC(connectionDetails = connectionDetails, 
                   cdmSchema = cdmSchema, 
                   resultsSchema = resultsSchema, 
                   sourceName = sourceName,
                   resultsTablePrefix = resultsTablePrefix, 
                   createResultsTable = createResultsTable, 
                   analysisId = analysisId,
                   comb = queryResults, 
                   multipleControlPeriods = multipleControlPeriods, 
                   multipleObservationPeriods = multipleObservationPeriods, 
                   shrinkage = shrinkage, 
                   icPercentile = icPercentile
                   )
  
  # -------------------------------
  # --    Store results in table --
  # -------------------------------
  #print(comb, quote = TRUE, row.names = FALSE)
  if(createResultsTable == TRUE)
  {
    renderedSql <- loadRenderTranslateSql(sqlFilename = "CreateResultsTable.sql",
                                          packageName = "IcTemporalPatternDiscovery",
                                          dbms = connectionDetails$dbms,
                                          resultsSchema = resultsSchema, 
                                          resultsTablePrefix = resultsTablePrefix, 
                                          sourceName = sourceName
    )
    
    writeLines(paste("Create results table", sep=""))
    #writeLines(paste(renderedSql, sep=""))
    #print(connectionDetails)
    executeSql(conn,renderedSql)
    sql <- c(sql,renderedSql)            
    
    for(cc in 1:length(comb[['CXY_CONTROL']]))
    {
      combItem <- comb[cc,]
      #print(combItem)
      renderedSql <- loadRenderTranslateSql(sqlFilename = "StoreResults.sql",
                                            packageName = "IcTemporalPatternDiscovery",
                                            dbms = connectionDetails$dbms,
                                            resultsSchema = resultsSchema, 
                                            resultsTablePrefix = resultsTablePrefix, 
                                            sourceName = sourceName,
                                            analysisId = analysisId,
                                            exposureOfInterest = combItem[['EXPOSUREOFINTEREST']],   
                                            outcomeOfInterest  = combItem[['OUTCOMEOFINTEREST']],   
                                            combinationCount                  = combItem[['CXY']],
                                            exposureCount                   = combItem[['CX']],
                                            outcomeCount                   = combItem[['CY']],
                                            totalCount                    = combItem[['C']],
                                            expected             = combItem[['expected']],
                                            ic                   = combItem[['IC']],
                                            lowIc                = combItem[['IC_low']],
                                            highIc               = combItem[['IC_high']]
      )
      
      writeLines(paste("Write results to table",sep=""))
      #writeLines(paste(renderedSql, sep=""))
      executeSql(conn,renderedSql)
      sql <- c(sql,renderedSql)            
    }
  } else
  {
    # TODO: Not yet implemented
    #writeIctpdAnalysesDetailsToFile(createIctpdAnalysisDetails(analysisId = analysisId),analysesDetails)
  }
  
  writeLines("Statistics finished")
  dbDisconnect(conn)
}



ic <- function(obs,exp,shape.add=0.5,rate.add=0.5, percentile=0.025) {
  ic    <- log2(                     (obs+shape.add)/     (exp+rate.add) )
  ic_low <- log2(qgamma(p=percentile    , shape=(obs+shape.add), rate=(exp+rate.add)))
  ic_high <- log2(qgamma(p=(1-percentile), shape=(obs+shape.add), rate=(exp+rate.add)))
  return(list(ic=ic,ic_low=ic_low,ic_high=ic_high))
}



CalcStatisticsIC <- function (connectionDetails, 
                              cdmSchema, 
                              resultsSchema, 
                              sourceName,
                              resultsTablePrefix,
                              createResultsTable,
                              analysisId, 
                              comb, 
                              multipleControlPeriods, 
                              multipleObservationPeriods, 
                              shrinkage, 
                              icPercentile
                              )
{
  for(controlPeriod in 1:length(multipleControlPeriods[[1]]))
  {
    expectedControl = c();
    
    controlPeriodItem=strsplit(toString(multipleControlPeriods[[1]][controlPeriod]),'');
    
    tmpMat <- matrix(rep(NA, length(comb[['CXY_CONTROL']])), ncol=1);
    
    if(controlPeriodItem[[1]][1] == 1)
    {
      tmpMat <- cbind(tmpMat, comb[['CXY_CONTROL']] / (comb[['CX_CONTROL']] * (comb[['CY_CONTROL']]  / comb[['C_CONTROL']])));
    }
    if(controlPeriodItem[[1]][2] == 1)
    {
      tmpMat <- cbind(tmpMat, comb[['CXY_1M']] / (comb[['CX_1M']] * (comb[['CY_1M']]  / comb[['C_1M']])));
    }
    if(controlPeriodItem[[1]][3] == 1)
    {
      tmpMat <- cbind(tmpMat, comb[['CXY_0M']] / (comb[['CX_0M']] * (comb[['CY_0M']]  / comb[['C_0M']])));
    }
    if(ncol(tmpMat) == 1)
    {
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
    for(obsPeriod in 1:length(multipleObservationPeriods[[1]]))
    {
      obsPeriodItem=strsplit(toString(multipleObservationPeriods[[1]][obsPeriod]),'');
      
      tmpMat <- matrix(rep(-99999999, length(comb[['CXY_CONTROL']])), ncol=1);
      
      comb['IC'] <- c();
      comb['IC_low'] <- c();
      comb['IC_high'] <- c();
      comb['CXY'] <- c();
      comb['CX'] <- c();
      comb['CY'] <- c();
      comb['C'] <- c();
      comb['expected'] <- c()
      
      for(cc in 1:length(comb[['CXY_CONTROL']]))
      {
        maxIC_low <- -999999999;
        maxIC <- NA;
        maxIC_high <- NA;
        CXY <- NA;
        CX <- NA;
        CY <- NA;
        C <- NA;
        expected <- NA;
        
        if(obsPeriodItem[[1]][1] == 1)
        {
          tmpIC <- ic(  comb[['CXY_OBSERVED_1_30']][cc]                                                               # Observed
                        ,(comb[['CX_OBSERVED_1_30']][cc] * (comb[['CY_OBSERVED_1_30']][cc] / comb[['C_OBSERVED_1_30']][cc]))     # Expected_observed
                        * expectedControl[cc]                                                                       	# Expected_control
                        , as.numeric(shrinkage)
                        , as.numeric(shrinkage)		                 									# Shrinkage factors
                        , as.numeric(icPercentile));
          
          if( ! is.na(tmpIC$ic_low))
          {
            if(maxIC_low < tmpIC$ic_low)
            {
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
        
        if(obsPeriodItem[[1]][2] == 1)
        {
          tmpIC <- ic(  comb[['CXY_OBSERVED_1_360']][cc]                                                               # Observed
                        ,(comb[['CX_OBSERVED_1_360']][cc] * (comb[['CY_OBSERVED_1_360']][cc] / comb[['C_OBSERVED_1_360']][cc]))   	# Expected_observed
                        * expectedControl[cc]                                                                        	# Expected_control
                        , as.numeric(shrinkage)
                        , as.numeric(shrinkage)  	                 									# Shrinkage factors
                        , as.numeric(icPercentile));
          if( ! is.na(tmpIC$ic_low))
          {
            if(maxIC_low < tmpIC$ic_low)
            {
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
        if(obsPeriodItem[[1]][3] == 1)
        {
          tmpIC <- ic(  comb[['CXY_OBSERVED_31_90']][cc]                                                               # Observed
                        ,(comb[['CX_OBSERVED_31_90']][cc] * (comb[['CY_OBSERVED_31_90']][cc] / comb[['C_OBSERVED_31_90']][cc]))   	# Expected_observed
                        * expectedControl[cc]                                                                        	# Expected_control
                        , as.numeric(shrinkage)
                        , as.numeric(shrinkage)  	                 									# Shrinkage factors
                        , as.numeric(icPercentile));
          if( ! is.na(tmpIC$ic_low))
          {
            if(maxIC_low < tmpIC$ic_low)
            {
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
        if(obsPeriodItem[[1]][4] == 1)
        {
          tmpIC <- ic(  comb[['CXY_OBSERVED_91_180']][cc]                                                               # Observed
                        ,(comb[['CX_OBSERVED_91_180']][cc] * (comb[['CY_OBSERVED_91_180']][cc] / comb[['C_OBSERVED_91_180']][cc]))   	# Expected_observed
                        * expectedControl[cc]                                                                        	# Expected_control
                        , as.numeric(shrinkage)
                        , as.numeric(shrinkage)  	                 									# Shrinkage factors
                        , as.numeric(icPercentile));
          if( ! is.na(tmpIC$ic_low))
          {
            if(maxIC_low < tmpIC$ic_low)
            {
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
        if(obsPeriodItem[[1]][5] == 1)
        {
          tmpIC <- ic(  comb[['CXY_OBSERVED_721_1080']][cc]                                                               # Observed
                        ,(comb[['CX_OBSERVED_721_1080']][cc] * (comb[['CY_OBSERVED_721_1080']][cc] / comb[['C_OBSERVED_721_1080']][cc]))   	# Expected_observed
                        * expectedControl[cc]                                                                        	# Expected_control
                        , as.numeric(shrinkage)
                        , as.numeric(shrinkage)  	                 									# Shrinkage factors
                        , as.numeric(icPercentile));
          if( ! is.na(tmpIC$ic_low))
          {
            if(maxIC_low < tmpIC$ic_low)
            {
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
        if(is.na(maxIC))
        {
          comb[['IC']][cc] <- NA;
          comb[['IC_low']][cc] <- NA;
          comb[['IC_high']][cc] <- NA;
          comb[['CXY']][cc] <- NA;
          comb[['CX']][cc]  <- NA;
          comb[['CY']][cc]  <- NA;
          comb[['C']][cc]   <- NA;
          comb[['expected']][cc]   <- NA;
        } else
        {
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
      return(comb)
      
    }

  }
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






