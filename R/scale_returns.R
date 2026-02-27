#' Scale Returns
#'
#' @param R
#' @param target
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
scale_returns <- function(R, target = NA, ...) {
  # ... your code ...
  curr_vol <- PerformanceAnalytics::sd.annualized(R)
  vol_ratio <- target / as.vector(curr_vol)
  R_scaled <- xts::xts(t(t(R) * vol_ratio), zoo::index(R))
  R_scaled

}

#
# xManagers <- scale_returns(managers,.14)
# managers / xManagers
# sd.annualized(managers)
#
# xxx <- merge(managers[,9],xManagers[,9])
# charts.RollingPerformance(xxx,width = 36)
#
