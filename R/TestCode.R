# Temporary placeholder for testing code until we figure out unit testing with DB and filesys dependencies
ictpdTestRoutines <- function(){
  setwd("c:/temp")
  connectionDetails <- createConnectionDetails(dbms="sql server", server="RNDUSRDHIT07.jnj.com")
  cdmDatabaseSchema = "cdm4_sim.dbo"
  resultsDatabaseSchema = "scratch.dbo"
    
  exposureOutcomePairs = data.frame(exposureConceptId = c(767410,1314924,907879,767410,1314924,907879,767410,1314924,907879), outcomeConceptId = c(444382, 444382, 444382,79106,79106,79106,138825,138825,138825))
  
  #Two steps:
  ictpdData <- getDbIctpdData(connectionDetails, cdmDatabaseSchema, resultsDatabaseSchema, exposureOutcomePairs)
  ictpdResults <- calculateStatisticsIc(ictpdData)
  ictpdResults
  summary(ictpdResults)
  
  #One step:
  ictpdAnalysesResults <- runIctpd(connectionDetails, cdmDatabaseSchema, resultsDatabaseSchema, exposureOutcomePairs)
  
  #One step, storing results on server:
  ictpdAnalysesResults <- runIctpd(connectionDetails, cdmDatabaseSchema, resultsDatabaseSchema, exposureOutcomePairs, storeResultsInDatabase = TRUE, createOutputTables = TRUE)
  
  #Using analyses:
  analysis <- createIctpdAnalysis(analysisId = 1, censor = TRUE)
  analysisList <- appendToIctpdAnalysisList(analysis)
  analysis <- createIctpdAnalysis(analysisId = 2, censor = FALSE)
  analysisList <- appendToIctpdAnalysisList(analysis, analysisList)
  setwd("c:/temp")
  writeIctpdAnalysisList(analysisList, "ictpdAnalysisList.csv")
  analysisList2 <- loadIctpdAnalysisList("ictpdAnalysisList.csv")
  
  #Test on postgres:
  library(IcTemporalPatternDiscovery)
  setwd("c:/temp")
  connectionDetails <- createConnectionDetails(dbms = "postgresql", user = "postgres", password = pw, server = "localhost/ohdsi")
  cdmDatabaseSchema = "cdm4_sim"
  resultsDatabaseSchema = "scratch"
  exposureOutcomePairs = data.frame(exposureConceptId = c(767410,1314924,907879,767410,1314924,907879,767410,1314924,907879), outcomeConceptId = c(444382, 444382, 444382,79106,79106,79106,138825,138825,138825))
  ictpdAnalysesResults <- runIctpd(connectionDetails, cdmDatabaseSchema, resultsDatabaseSchema, exposureOutcomePairs)
  save(ictpdAnalysesResults, file = "c:/temp/ictpdAnalysesResults.rda")
}