test_that("Eunomia example works", {
  
  library(Eunomia)
  connectionDetails <- getEunomiaConnectionDetails()
  exposureIds = c(738818)
  outcomeIds = c(260139)
  exposureOutcomePairs = data.frame("exposureId" = exposureIds, "outcomeId" = outcomeIds)
  exposureOutcomePairs$grouping = 1
  cdmDatabaseSchema = "main"
  pathToSql <- system.file("sql/sql_server",
                           "SQLITE_CreateChronographDataFromExposureOutcomePairs.sql",
                           package = "IcTemporalPatternDiscovery")
  
  expect_equal(file.info(pathToSql)$size, 9936)
})
