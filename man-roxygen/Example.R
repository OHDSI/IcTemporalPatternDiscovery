#' @examples \dontrun{
#' connectionDetails <- createConnectionDetails(dbms="sql server", server="server_ip")
#' exposureOutcomePairs = data.frame(outcomeConceptId = c(196794, 196794, 312648), exposureConceptId = c(1501700, 1545958, 1551803))
#' ictpdData <- getDbIctpdData(connectionDetails, cdmDatabaseSchema = "cdm_schema.dbo", resultsDatabaseSchema = "results.dbo", exposureOutcomePairs = exposureOutcomePairs)
#' ictpdResults <- calculateStatisticsIC(ictpdData)
#' ictpdResults
#' }
