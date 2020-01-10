test_that("Chronograph works", {
  
  sql <- SqlRender::loadRenderTranslateSql("test.sql", dbms="sql server", packageName="IcTemporalPatternDiscovery")
  
  # Bug reported at https://github.com/OHDSI/SqlRender/issues/205
  # library(Eunomia)
  # connectionDetails <- Eunomia::getEunomiaConnectionDetails()
  # exposureIds = c(738818)
  # outcomeIds = c(260139)
  # exposureOutcomePairs = data.frame("exposureId" = exposureIds, "outcomeId" = outcomeIds)
  # exposureOutcomePairs$grouping = 1
  # cdmDatabaseSchema = "main"
  # 
  # result <- IcTemporalPatternDiscovery::getChronographData(connectionDetails,
  #                                                          cdmDatabaseSchema = "main",
  #                                                          oracleTempSchema = NULL,
  #                                                          cdmVersion = "5",
  #                                                          exposureIds = NULL,
  #                                                          outcomeIds = outcomeIds,
  #                                                          exposureOutcomePairs = exposureOutcomePairs,
  #                                                          exposureDatabaseSchema = cdmDatabaseSchema,
  #                                                          exposureTable = "drug_era",
  #                                                          outcomeDatabaseSchema = cdmDatabaseSchema,
  #                                                          outcomeTable = "condition_era",
  #                                                          shrinkage = 0.5,
  #                                                          icPercentile = 0.025,
  #                                                          randomSample = NULL,
  #                                                          patientLevel = TRUE)
  # 
  # expect_equal(result$outcomeCount[result$periodId==0], 1974)
})
