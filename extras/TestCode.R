library(IcTemporalPatternDiscovery)

connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = "redshift",
                                                                connectionString = keyring::key_get("redShiftConnectionStringOhdaMdcd"),
                                                                user = keyring::key_get("redShiftUserName"),
                                                                password = keyring::key_get("redShiftPassword"))
cdmDatabaseSchema <- "cdm_truven_mdcd_v2128"
cohortDatabaseSchema <- "scratch_mschuemi"
cdmVersion <- "5"

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
                            exposureOutcomePairs = exposureOutcomePairs)
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

# Chronographs --------------------------------------------------------------

connection <- DatabaseConnector::connect(connectionDetails)

# Create GI Bleed outcome
sql <- SqlRender::loadRenderTranslateSql("VignetteOutcomes.sql",
                                         packageName = "CohortMethod",
                                         dbms = connection@dbms,
                                         cdmDatabaseSchema = cdmDatabaseSchema,
                                         resultsDatabaseSchema = cohortDatabaseSchema)
DatabaseConnector::executeSql(connection, sql)

# Create custom exposure table

sql <- "
DROP TABLE IF EXISTS @cohort_database_schema.@exposure_table;

SELECT subject_id,
  cohort_definition_id,
  cohort_start_date
INTO @cohort_database_schema.@exposure_table
FROM (
SELECT person_id AS subject_id,
  drug_concept_id AS cohort_definition_id,
  MIN(drug_era_start_date) AS cohort_start_date
FROM @cdm_database_schema.drug_era
INNER JOIN @cdm_database_schema.concept_ancestor
ON drug_concept_id = descendant_concept_id
WHERE ancestor_concept_id = 21603933
GROUP BY person_id,
  drug_concept_id
) first_exposure
INNER JOIN @cdm_database_schema.observation_period
ON subject_id = person_id
AND cohort_start_date >= observation_period_start_date
AND cohort_start_date <= observation_period_end_date
WHERE DATEDIFF(DAY, observation_period_start_date, cohort_start_date) >= 365;"

sql <- SqlRender::render(
  sql = sql, 
  cohort_database_schema = cohortDatabaseSchema, 
  cdm_database_schema = cdmDatabaseSchema,
  exposure_table = "mschuemi_test_exposures"
)
sql <- SqlRender::translate(
  sql = sql, 
  targetDialect = connectionDetails$dbms
)
DatabaseConnector::executeSql(connection, sql)

sql <- "SELECT cohort_definition_id,
  COUNT(*) AS cohort_count
FROM @cohort_database_schema.@cohort_table
GROUP BY cohort_definition_id
ORDER BY -COUNT(*);"
renderTranslateQuerySql(
  connection = connection,
  sql = sql,
  snakeCaseToCamelCase = TRUE,
  cohort_database_schema = cohortDatabaseSchema,
  cohort_table = "mschuemi_test_exposures"
)
renderTranslateQuerySql(
  connection = connection,
  sql = sql,
  snakeCaseToCamelCase = TRUE,
  cohort_database_schema = cohortDatabaseSchema,
  cohort_table = "outcomes"
)

DatabaseConnector::disconnect(connection)

chronographData <- getChronographData(
  connectionDetails = connectionDetails,
  cdmDatabaseSchema = cdmDatabaseSchema,
  exposureOutcomePairs = data.frame(exposureId = c(1124300, 1118084),
                                    outcomeId = c(192671, 192671)),
  exposureDatabaseSchema = cohortDatabaseSchema,
  exposureTable = "mschuemi_test_exposures",
  outcomeDatabaseSchema = cohortDatabaseSchema,
  outcomeTable = "outcomes"
)

saveRDS(chronographData, "s:/temp/chronographData.rds")

plotChronograph(chronographData, exposureId = 1124300, outcomeId = 192671)

# exposureIds = c(19088915, 19072152, 19042155, 19095703, 19029393, 19029398, 19086910, 1197736, 19003570, 19041220, 19056874, 19071933, 19011355, 19056645, 19136654, 19102108, 19110711, 19018431, 19039703, 19069191, 19049709, 19029327, 19082874, 1118045, 1118084, 1153928, 1177480, 1178663, 1146810, 1236607, 1360332, 1036636, 1156378, 1113648, 1135710, 1189754, 1103374, 1124300, 1136980, 1185922, 1195492, 1115008, 1150345, 1395573)
# outcomeIds = c(192671)
# exposureDatabaseSchema = cdmDatabaseSchema
# exposureTable = "drug_era"
# outcomeDatabaseSchema = outcomeDatabaseSchema
# outcomeTable = "outcomes"
# 
# exposureId <- 1124300 # Diclofenac
# exposureId <- 1118084 # Celecoxib
# outcomeId <- 192671
# 
# OhdsiRTools::formatRFile("R/Chronograph.r")
# 
# 
# 
# chronographData <- getChronographData(connectionDetails = connectionDetails,
#                                       cdmDatabaseSchema = "cdm_truven_mdcd_v699.dbo",
#                                       oracleTempSchema = NULL,
#                                       exposureIds = c(),
#                                       outcomeIds = c(2551, 2556, 2557, 2559, 2820, 2821, 2822, 2823, 2824, 2826, 2828, 2829, 2830, 2831, 2832, 2833, 2834, 2835, 2836, 2837, 2838, 2839),
#                                       exposureDatabaseSchema = "scratch.dbo",
#                                       exposureTable = "legend_mdcd_depression_exp_cohort",
#                                       outcomeDatabaseSchema = "scratch.dbo",
#                                       outcomeTable = "legend_mdcd_depression_out_cohort")
# plotChronograph(chronographData, exposureId = 739138, outcomeId = 2820)
