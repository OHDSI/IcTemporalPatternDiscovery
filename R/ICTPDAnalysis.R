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

#' @title createSccAnalysisDetails
#'
#' @description
#' \code{createIctpdAnalysis} generates an object specifying one set of analysis choices
#' for the IC Temporal Pattern Discovery method.
#'  
#' @param analysisId  	A unique identifier that can later be used to identify the results of this analysis

#' @template GetDataParameters
#' 
#' @template IcCalculationParameters
#' 
#' @template ExampleUsingAnalysis
#' 
#' @export
createIctpdAnalysis <- function(analysisId = 1,
                                
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
  #First: get the default values:
  analysis <- list()
  for (name in names(formals(createIctpdAnalysis))){
    analysis[[name]] = get(name)
  }
  
  #Next: overwrite defaults with actual values if specified:
  #values <- as.list(match.call())
  #Note: need this funky code to make sure parameters are stored as values, not symbols:
  values <- c(list(as.character(match.call()[[1]])),lapply(as.list(match.call())[-1],function(x) eval(x,envir=sys.frame(-3))))
  for (name in names(values)){
    if (name %in% names(analysis))
      analysis[[name]] = values[[name]]
  }
  
  class(analysis) <- "ictpdAnalysis"
  analysis
}

#' @title Append to list of ictpdAnalysis
#'
#' @description
#' \code{appendToIctpdAnalysisList} adds an object of type \code{ictpdAnalysis} to a list.
#'  
#' @param ictpdAnalysisList    The list to add the object to. If not provided, a new list will be created.
#' @param ictpdAnalysis    Object to append to the list.
#' 
#' @return A list of objects of type \code{ictpdAnalysis}.
#' 
#' @template ExampleUsingAnalysis
#' 
#' @export
appendToIctpdAnalysisList <- function(ictpdAnalysis, ictpdAnalysisList = NULL){
  stopifnot(class(ictpdAnalysis) == "ictpdAnalysis")
  if (is.null(ictpdAnalysisList)){
    ictpdAnalysisList = list()
  }
  ictpdAnalysisList[[length(ictpdAnalysisList)+1]] <- ictpdAnalysis
  return(ictpdAnalysisList)
}

.convertAnalysisListToDataFrame <- function(f){
  d <- data.frame()
  for (row in 1:length(f)){
    class(f[[row]]) <- "list"
    for (column in 1:length(f[[row]])){
      if ((class(f[[row]][[column]]) == "numeric") && (length(f[[row]][[column]]) > 1))
        f[[row]][[column]] = paste(f[[row]][[column]],collapse=";")
    }
    d <- rbind(d,as.data.frame(f[[row]]))
  }
  return(d)
}

#' @title writeIctpdAnalysisListToFile
#'
#' @description
#' \code{writeIctpdAnalysisListToFile} writes an object of type \code{IctpdAnalysisList} to a CSV file
#'  
#' @param ictpdAnalysisList    the \code{ictpdAnalysisList} to be written to file
#' @param file                  the name of the file where the results will be written
#' 
#' @template ExampleUsingAnalysis
#' 
#' @export
writeIctpdAnalysisList <- function(ictpdAnalysisList, file){
  stopifnot(class(ictpdAnalysisList) == "list")
  stopifnot(length(ictpdAnalysisList) > 0)
  for (i in 1:length(ictpdAnalysisList)){
    stopifnot(class(ictpdAnalysisList[[1]]) == "ictpdAnalysis")
  }
  
  d <- .convertAnalysisListToDataFrame(ictpdAnalysisList)
 
  write.csv(d,file=file, row.names=FALSE)
}


#' @title Load a list of Ictpd analysis from file
#'
#' @description
#' \code{loadIctpdAnalysisList} reads a list of objects of type \code{ictpdAnalysis} from a CSV file
#'  
#' @param file                  the name of the file to be loaded
#' 
#' @return A list of objects of type \code{ictpdAnalysis} 
#' 
#' @template ExampleUsingAnalysis
#' 
#' @export
loadIctpdAnalysisList <- function(file){
  d <- read.csv(file)
  d[is.na(d)] <- ""
  ictpdAnalysisList <- list()
  for (row in 1:nrow(d)){
    ictpdAnalysis <- as.list(d[row,])
    #for (column in c("drugTypeConceptIdList","conditionTypeConceptIdList")){
    #  ictpdAnalysis[[column]] <- as.numeric(unlist(strsplit(as.character(d[row,column]),";")))
    #}
    class(ictpdAnalysis) = "ictpdAnalysis"
    ictpdAnalysisList[[length(ictpdAnalysisList)+1]] <- ictpdAnalysis
  }
  return(ictpdAnalysisList)
}





