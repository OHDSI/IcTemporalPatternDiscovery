% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RunAnalyses.R
\name{runIctpdAnalyses}
\alias{runIctpdAnalyses}
\title{Run a list of analyses}
\usage{
runIctpdAnalyses(
  connectionDetails,
  cdmDatabaseSchema,
  oracleTempSchema = cdmDatabaseSchema,
  exposureDatabaseSchema = cdmDatabaseSchema,
  exposureTable = "drug_era",
  outcomeDatabaseSchema = cdmDatabaseSchema,
  outcomeTable = "condition_era",
  cdmVersion = 4,
  outputFolder = "./IctpdOutput",
  ictpdAnalysisList,
  exposureOutcomeList,
  getDbIctpdDataThreads = 1,
  calculateStatisticsIcThreads = 1
)
}
\arguments{
\item{connectionDetails}{An R object of type \code{connectionDetails} created using the
function \code{createConnectionDetails} in the
\code{DatabaseConnector} package.}

\item{cdmDatabaseSchema}{The name of the database schema that contains the OMOP CDM
instance. Requires read permissions to this database. On SQL
Server, this should specifiy both the database and the schema,
so for example 'cdm_instance.dbo'.}

\item{oracleTempSchema}{For Oracle only: the name of the database schema where you
want all temporary tables to be managed. Requires
create/insert permissions to this database.}

\item{exposureDatabaseSchema}{The name of the database schema that is the location where the
exposure data used to define the exposure cohorts is
available. If exposureTable = DRUG_ERA, exposureDatabaseSchema
is not used by assumed to be cdmSchema.  Requires read
permissions to this database.}

\item{exposureTable}{The tablename that contains the exposure cohorts.  If
exposureTable <> DRUG_ERA, then expectation is exposureTable
has format of COHORT table: COHORT_DEFINITION_ID, SUBJECT_ID,
COHORT_START_DATE, COHORT_END_DATE.}

\item{outcomeDatabaseSchema}{The name of the database schema that is the location where the
data used to define the outcome cohorts is available. If
exposureTable = CONDITION_ERA, exposureDatabaseSchema is not
used by assumed to be cdmSchema.  Requires read permissions to
this database.}

\item{outcomeTable}{The tablename that contains the outcome cohorts.  If
outcomeTable <> CONDITION_OCCURRENCE, then expectation is
outcomeTable has format of COHORT table: COHORT_DEFINITION_ID,
SUBJECT_ID, COHORT_START_DATE, COHORT_END_DATE.}

\item{cdmVersion}{Define the OMOP CDM version used: currently support "4" and
"5".}

\item{outputFolder}{Name of the folder where all the outputs will written to.}

\item{ictpdAnalysisList}{A list of objects of type \code{ictpdAnalysis} as created
using the \code{\link{createIctpdAnalysis}} function.}

\item{exposureOutcomeList}{A list of objects of type \code{exposureOutcome} as created
using the \code{\link{createExposureOutcome}} function.}

\item{getDbIctpdDataThreads}{The number of parallel threads to use to load the data from
the database.}

\item{calculateStatisticsIcThreads}{The number of threads used to perform the IC statistics
computations.}
}
\description{
Run a list of analyses
}
\details{
Run a list of analyses for the exposure-outcomes of interest. This function will run all specified
analyses against all hypotheses of interest, meaning that the total number of outcome models is
`length(ictpdAnalysisList) * length(exposureOutcomeList)`. When you provide several analyses it
will determine whether any of the analyses have anything in common, and will take advantage of this
fact. For example, if we specify several analyses that only differ in the way the outcome model is
fitted, then this function will extract the data and fit the propensity model only once, and re-use
this in all the analysis.
}
