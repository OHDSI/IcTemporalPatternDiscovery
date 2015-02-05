#' @param riskPeriodStart start of the risk period - can be set between 0 and 99999, default is 1.
#' @param riskPeriodEnd end of the risk period - can be set between 0 and 99999, default is 30.
#' @param controlPeriodStart start of the control period - can be set between -99999 and 0, default is -1080.
#' @param controlPeriodEnd end of the control period - can be set between -99999 and 0, default is -361.
#' @param censor a flag indicating wether the method should censor the observation period at the end of exposure or not. Available input is 0 or 1 with default = 0. 
