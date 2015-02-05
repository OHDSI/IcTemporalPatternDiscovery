#' @param shrinkage               Shrinkage used in IRR calculations, required >0 to deal with 0 case counts, but larger number means more shrinkage. default is 0.5
#' @param icPercentile            The lower bound of the credibility interval for the IC values (IClow). default is 0.025,
#' @param metric                  Defines wether the output will contain the point estimate or the lower bound. Available input is 'IC and 'IC025' default is 'IC025'
#' @param multipleControlPeriods  Defines the control periods to use where 100 means the control period defined by controlPeriodStart/End, 010 means the period -30 to -1 day before prescription and 001 means the control period on the day of prescription
#' @param multipleRiskPeriods     Defines the risk periods to use 10000 is 1-30 days, 01000 is 1 to 360 days, 00100 is 31 to 90 days, 00010 is 91 to 180 and 00001 is 721 to 1080 days after prescription default is '10000'
