#' Title
#'
#' @param R
#' @param geometric
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
scale_returns <- function(R, target = NA, ...) {
  # ... your code ...
  curr_vol <- sd.annualized(R)
  vol_ratio <- target / as.vector(curr_vol)
  R_scaled <- xts(t(t(R) * vol_ratio), index(R))
  R_scaled

}


xManagers <- scale_returns(managers,.14)
managers / xManagers
sd.annualized(managers)
