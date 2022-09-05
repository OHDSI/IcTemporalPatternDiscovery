library(testthat)
library(IcTemporalPatternDiscovery)

test_that("Single-study mode", {
  exposureOutcomePairs <- data.frame(
    exposureId = c(1, 2, 3),
    outcomeId = c(4, 4, 4)
  )

  ictpdData <- getDbIctpdData(
    connectionDetails = connectionDetails,
    cdmDatabaseSchema = "main",
    exposureDatabaseSchema = "main",
    exposureTable = "cohort",
    outcomeDatabaseSchema = "main",
    outcomeTable = "cohort",
    exposureOutcomePairs = exposureOutcomePairs,
    controlPeriodStart = -30,
    controlPeriodEnd = -1,
    riskPeriodStart = 1,
    riskPeriodEnd = 30
  )
  ictpdResults <- calculateStatisticsIc(ictpdData)
  expect_s3_class(ictpdResults, "ictpdResults")
})

test_that("Multi-analysis mode", {
  outputFolder <- tempfile("ictpd")
  dir.create(outputFolder)
  on.exit(unlink(outputFolder, recursive = TRUE))

  getDbIctpdDataArgs1 <- createGetDbIctpdDataArgs(censor = TRUE)
  getDbIctpdDataArgs2 <- createGetDbIctpdDataArgs(censor = FALSE)
  calculateStatisticsIcArgs <- createCalculateStatisticsIcArgs()
  analysis1 <- createIctpdAnalysis(
    analysisId = 1,
    getDbIctpdDataArgs = getDbIctpdDataArgs1,
    calculateStatisticsIcArgs = calculateStatisticsIcArgs
  )
  analysis2 <- createIctpdAnalysis(
    analysisId = 2,
    getDbIctpdDataArgs = getDbIctpdDataArgs2,
    calculateStatisticsIcArgs = calculateStatisticsIcArgs
  )
  ictpdAnalysisList <- list(analysis1, analysis2)

  exposureOutcomePairs <- data.frame(
    exposureId = c(1, 2, 3),
    outcomeId = c(4, 4, 4)
  )
  exposureOutcomeList <- apply(exposureOutcomePairs, 1, function(x) createExposureOutcome(x[1], x[2]))
  result <- runIctpdAnalyses(
    connectionDetails = connectionDetails,
    cdmDatabaseSchema = "main",
    exposureDatabaseSchema = "main",
    exposureTable = "cohort",
    outcomeDatabaseSchema = "main",
    outcomeTable = "cohort",
    outputFolder = outputFolder,
    ictpdAnalysisList = ictpdAnalysisList,
    exposureOutcomeList = exposureOutcomeList
  )
  expect_gt(nrow(result), 1)
})

test_that("Chronograph", {
  exposureOutcomePairs <- data.frame(
    exposureId = c(1, 2, 3),
    outcomeId = c(4, 4, 4)
  )

  chronographData <- getChronographData(
    connectionDetails = connectionDetails,
    cdmDatabaseSchema = "main",
    exposureDatabaseSchema = "main",
    exposureTable = "cohort",
    outcomeDatabaseSchema = "main",
    outcomeTable = "cohort",
    exposureOutcomePairs = exposureOutcomePairs
  )
  expect_gt(nrow(chronographData), 1)

  plot <- plotChronograph(
    data = chronographData,
    exposureId = 1,
    outcomeId = 4
  )
  expect_is(plot, "gtable")
})
