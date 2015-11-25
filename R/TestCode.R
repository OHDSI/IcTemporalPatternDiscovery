.ictpdTestRoutines <- function() {
  pw <- NULL
  dbms <- "sql server"
  user <- NULL
  server <- "RNDUSRDHIT07.jnj.com"
  cdmDatabaseSchema <- "cdm4_sim.dbo"
  oracleTempSchema <- NULL
  port <- NULL
  cdmVersion <- 4

  dbms <- "postgresql"
  user <- "postgres"
  server <- "localhost/ohdsi"
  cdmDatabaseSchema <- "vocabulary5"
  oracleTempSchema <- NULL
  port <- NULL
  cdmVersion <- 5

  pw <- NULL
  dbms <- "pdw"
  user <- NULL
  server <- "JRDUSAPSCTL01"
  cdmDatabaseSchema <- "cdm_truven_mdcd.dbo"
  oracleTempSchema <- NULL
  port <- 17001
  cdmVersion <- 4

  connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = dbms,
                                                                  server = server,
                                                                  user = user,
                                                                  password = pw,
                                                                  port = port)

  exposureOutcomePairs <- data.frame(exposureId = c(767410,
                                                    1314924,
                                                    907879,
                                                    767410,
                                                    1314924,
                                                    907879,
                                                    767410,
                                                    1314924,
                                                    907879),
                                     outcomeId = c(444382,
                                                   444382,
                                                   444382,
                                                   79106,
                                                   79106,
                                                   79106,
                                                   138825,
                                                   138825,
                                                   138825))

  # Two steps:
  ictpdData <- getDbIctpdData(connectionDetails = connectionDetails,
                              cdmDatabaseSchema = cdmDatabaseSchema,
                              exposureOutcomePairs = exposureOutcomePairs,
                              cdmVersion = cdmVersion)
  ictpdResults <- calculateStatisticsIc(ictpdData)
  ictpdResults
  summary(ictpdResults)

  # Using analyses:
  getDbIctpdDataArgs1 <- createGetDbIctpdDataArgs(censor = TRUE)
  getDbIctpdDataArgs2 <- createGetDbIctpdDataArgs(censor = FALSE)
  calculateStatisticsIcArgs <- createCalculateStatisticsIcArgs()
  analysis1 <- createIctpdAnalysis(analysisId = 1,
                                   getDbIctpdDataArgs = getDbIctpdDataArgs1,
                                   calculateStatisticsIcArgs = calculateStatisticsIcArgs)
  analysis2 <- createIctpdAnalysis(analysisId = 2,
                                   getDbIctpdDataArgs = getDbIctpdDataArgs2,
                                   calculateStatisticsIcArgs = calculateStatisticsIcArgs)
  ictpdAnalysisList <- list(analysis1, analysis2)

  saveIctpdAnalysisList(ictpdAnalysisList, "s:/temp/ictpdAnalysisList.txt")
  ictpdAnalysisList2 <- loadIctpdAnalysisList("s:/temp/ictpdAnalysisList.txt")

  exposureOutcomeList <- apply(exposureOutcomePairs, 1, function(x) createExposureOutcome(x[1],
                                                                                          x[2]))

  result <- runIctpdAnalyses(connectionDetails = connectionDetails,
                             cdmDatabaseSchema = cdmDatabaseSchema,
                             oracleTempSchema = oracleTempSchema,
                             cdmVersion = cdmVersion,
                             outputFolder = "s:/temp/ictpdResults",
                             ictpdAnalysisList = ictpdAnalysisList,
                             exposureOutcomeList = exposureOutcomeList)
  result <- readRDS("s:/temp/ictpdResults/resultsReference.rds")
  s <- summarizeAnalyses(result)

}
