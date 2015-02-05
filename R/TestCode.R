# Temporary placeholder for testing code until we figure out unit testing with DB and filesys dependencies
ictpdTestRoutines <- function(){
  setwd("c:/temp")
  connectionDetails <- createConnectionDetails(dbms="sql server", server="RNDUSRDHIT07.jnj.com")
  cdmDatabaseSchema = "cdm4_sim.dbo"
  resultsDatabaseSchema = "scratch.dbo"
    
  exposureOutcomePairs = data.frame(exposureConceptId = c(767410,1314924,907879,767410,1314924,907879,767410,1314924,907879), outcomeConceptId = c(444382, 444382, 444382,79106,79106,79106,138825,138825,138825))
  
  #Two steps:
  ictpdData <- getDbIctpdData(connectionDetails, cdmDatabaseSchema, resultsDatabaseSchema, exposureOutcomePairs)
  ictpdResults <- calculateStatisticsIC(ictpdData)
  
  #One step:
  ictpdResults <- runIctpd(connectionDetails, cdmDatabaseSchema, resultsDatabaseSchema, exposureOutcomePairs)
  
  #One step, storing results on server:
  ictpdResults <- runIctpd(connectionDetails, cdmDatabaseSchema, resultsDatabaseSchema, exposureOutcomePairs, storeResultsInDatabase = TRUE, createOutputTables = TRUE)
  
}