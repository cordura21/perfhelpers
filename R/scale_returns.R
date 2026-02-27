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

  curr_vol <- PerformanceAnalytics::sd.annualized(R)
  vol_ratio <- target / as.vector(curr_vol)
  if(is.na(target)){
    vol_ratio <- 1
  }
  R_scaled <- xts::xts(t(t(R) * vol_ratio), zoo::index(R))
  R_scaled

}

