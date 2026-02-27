#' Return statistics
#'
#' A simple helper that combines `Return.annualized()` and `maxDrawdown()`
#' from PerformanceAnalytics into one clean table.
#'
#' @param R An xts object of asset returns (can be one or many columns).
#' @param geometric Logical. Use geometric (TRUE) or arithmetic returns? Default TRUE.
#' @param ... Additional arguments passed to `Return.annualized()` and `maxDrawdown()`.
#'
#' @return A data.frame with one row per asset.
#' @export
#'
#' @examples
#' library(PerformanceAnalytics)
#' data(managers)
#' R <- managers[, 1:3]
#' return_stats(R)
return_stats <- function(R, geometric = TRUE, ...) {
  ann_ret <- PerformanceAnalytics::Return.annualized(R, geometric = geometric, ...)
  mdd     <- PerformanceAnalytics::maxDrawdown(R, ...)
  sd <- PerformanceAnalytics::sd.annualized(R,...)
  sr <- PerformanceAnalytics::SharpeRatio.annualized(R,...)
  calmar <- PerformanceAnalytics::CalmarRatio(R,)

  data.frame(
    Annualized.Return = as.vector(ann_ret),
    Max.Drawdown      = as.vector(mdd),
    SD.Annualized = as.vector(sd),
    sharpe = as.vector(sr),
    calmar = as.vector(calmar),
    start = xts::first(zoo::index(R)),
    end = xts::last(zoo::index(R)),
    length = nrow(R),
    nas = colSums(is.na(R)),
    periodicity =as.vector( xts::periodicity(R)$scale),
    row.names = if (is.null(colnames(R))) "Asset" else colnames(R)
  )
}
