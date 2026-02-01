


#' @post /update_monetary_policy_shock_data
function(){
  source("MonetaryPolicyShocksEstimation.R")
  print('updating monetary policy shocks dataset')
}

#' @post /update_underlying_data
function(){
  source("BoRSiteParser.R")
  source("YieldCurveConstruction.R")
  print('updating monetary policy shocks dataset')
}
print('app.R running')
