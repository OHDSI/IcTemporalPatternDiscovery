#' @examples
#' \dontrun{
#' library(SelfControlledCohort)
#'
#' connectionDetails <- createConnectionDetails(dbms = "postgresql",
#'                                              user = "joe",
#'                                              password = "secret",
#'                                              server = "myserver")
#' exposureOutcomePairs <- data.frame(outcomeId = c(196794, 196794, 312648),
#'                                    exposurId = c(1501700, 1545958, 1551803))
#' ictpdData <- getDbIctpdData(connectionDetails,
#'                             cdmDatabaseSchema = "cdm_schema.dbo",
#'                             exposureOutcomePairs = exposureOutcomePairs)
#' ictpdResults <- calculateStatisticsIC(ictpdData)
#' ictpdResults
#' }
