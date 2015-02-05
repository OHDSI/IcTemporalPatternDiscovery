/*********************************
IC temporal pattern discovery
a Self controlled cohort design

Store analysis details and results

Tomas Bergvall
**********************************/

USE @resultsDatabase;

{createOutputTables} ? {
IF OBJECT_ID('@outputTablePrefix_analysis', 'U') IS NOT NULL 
  DROP TABLE @outputTablePrefix_analysis;

CREATE TABLE @outputTablePrefix_analysis (
  analysisId INT NOT NULL,
  parameterName varchar(255) NOT NULL,
  value varchar(max),
  CONSTRAINT pk_@outputTablePrefix_analysis PRIMARY KEY (analysisId, parameterName)
);
} : {
DELETE FROM 
  @resultsTablePrefix_analysis 
WHERE analysisId = @analysisId;
}

INSERT INTO
	@outputTablePrefix_analysis
SELECT @analysisId, 'riskPeriodStart', '@riskPeriodStart'
UNION SELECT @analysisId, 'riskPeriodEnd', '@riskPeriodEnd'
UNION SELECT @analysisId, 'controlPeriodStart', '@controlPeriodStart'
UNION SELECT @analysisId, 'controlPeriodEnd', '@controlPeriodEnd'
UNION SELECT @analysisId, 'censor', '@censor'
UNION SELECT @analysisId, 'multipleControlPeriods', '@multipleControlPeriods'
UNION SELECT @analysisId, 'multipleRiskPeriods', '@multipleRiskPeriods'
UNION SELECT @analysisId, 'shrinkage', '@shrinkage'
UNION SELECT @analysisId, 'icPercentile', '@icPercentile'
UNION SELECT @analysisId, 'metric', '@metric'
;
