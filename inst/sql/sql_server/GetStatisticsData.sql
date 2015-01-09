/*****************
IC temporal pattern discovery
a Self controlled cohort design

Extract results from temporary table to calculate IC values

Tomas Bergvall
******************/

{DEFAULT @analysisId = 1}
{DEFAULT @exposureConceptId = "drug_concept_id"}
{DEFAULT @outcomeConceptId = "condition_concept_id"}
{DEFAULT @cdmSchema = ""}

USE @cdmSchema

SELECT
      TXY.@exposureConceptId as exposureOfInterest,
      TXY.@outcomeConceptId  as outcomeOfInterest,
      cxy_control,
      cx_control,
      cy_control,
      c_control,
      cxy_1m,
      cx_1m,
      cy_1m,
      c_1m,
      cxy_0m,
      cx_0m,
      cy_0m,
      c_0m,
    cxy_observed_1_30,
      cx_observed_1_30,
      cy_observed_1_30,
      c_observed_1_30,
    cxy_observed_1_360,
      cx_observed_1_360,
      cy_observed_1_360,
      c_observed_1_360,
    cxy_observed_31_90,
      cx_observed_31_90,
      cy_observed_31_90,
      c_observed_31_90,
    cxy_observed_91_180,
      cx_observed_91_180,
      cy_observed_91_180,
      c_observed_91_180,
    cxy_observed_721_1080,
      cx_observed_721_1080,
      cy_observed_721_1080,
      c_observed_721_1080
FROM BASE_LINE_COUNTER_CXY TXY
JOIN BASE_LINE_COUNTER_CX   TX ON TXY.AnalysisId = TX.AnalysisId AND TXY.@exposureConceptId = TX.@exposureConceptId  
JOIN BASE_LINE_COUNTER_CY   TY ON TXY.AnalysisId = TY.AnalysisId AND TXY.@outcomeConceptId  = TY.@outcomeConceptId  
JOIN BASE_LINE_COUNTER_C     T ON TXY.AnalysisId = T.AnalysisId
WHERE TXY.AnalysisId = @analysisId
ORDER BY TXY.@exposureConceptId,TXY.@outcomeConceptId

