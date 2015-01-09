/*****************
IC temporal pattern discovery
a Self controlled cohort design

Store analysis details and results

Tomas Bergvall
******************/

{DEFAULT @analysisId = 1}
{DEFAULT @sourceName = ""}
{DEFAULT @resultsSchema = ""}
{DEFAULT @resultsTablePrefix = "ictpd"}
{DEFAULT @exposureOfInterest = 0}
{DEFAULT @outcomeOfInterest = 0}
{DEFAULT @combinationCount = 0}                  
{DEFAULT @exposureCount = 0}                  
{DEFAULT @outcomeCount = 0}                  
{DEFAULT @totalCount = 0}                   
{DEFAULT @expected = 0}            
{DEFAULT @highIc = 0} 
{DEFAULT @lowIc = 0}   
{DEFAULT @ic = 0}                  
            
             

USE @resultsSchema;

DELETE FROM   
@resultsTablePrefix_results 
WHERE analysisId = @analysisId
AND sourceName =  '@sourceName'
AND  exposureConceptId = @exposureOfInterest
AND outcomeConceptId = @outcomeOfInterest;

INSERT INTO @resultsTablePrefix_results
SELECT 
@analysisId          
,'@sourceName'          
,@exposureOfInterest   
,@outcomeOfInterest    
,@combinationCount                 
,@exposureCount                  
,@outcomeCount                  
,@totalCount                   
,@expected            
,@ic                  
,@lowIc               
,@highIc              
