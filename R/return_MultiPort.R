# R/portfolio-multi.R

#' Compute portfolio returns for multiple strategies defined in a data frame
#'
#' @param returns_xts An xts object containing asset returns (columns = assets)
#' @param strategies_df A data frame with columns:
#'   \itemize{
#'     \item \code{strategy_name} (character) – name of the strategy
#'     \item \code{rebalance_on} ("monthly", "quarterly", "yearly", etc.)
#'     \item weight columns (w1, w2, …) – numeric weights for each asset
#'   }
#' @param ... Additional arguments passed to \code{PerformanceAnalytics::Return.portfolio()}
#'
#' @return An xts object with one column per strategy–rebalancing combination
#' @export
#' @importFrom xts xts
#' @importFrom PerformanceAnalytics Return.portfolio


Return.portfolio.multi <- function(returns_xts, strategies_df, ...) {

  stopifnot(inherits(returns_xts, "xts"))
  stopifnot(is.data.frame(strategies_df))
  stopifnot("strategy_name" %in% names(strategies_df))
  stopifnot("rebalance_on"  %in% names(strategies_df))

  # Weight columns = everything except the metadata columns
  weight_cols <- setdiff(names(strategies_df), c("strategy_name", "rebalance_on"))
  n_assets <- length(weight_cols)

  if (n_assets != ncol(returns_xts)) {
    stop("Number of weight columns (", n_assets, ") ≠ number of assets (", ncol(returns_xts), ")")
  }

  results <- list()

  for (i in seq_len(nrow(strategies_df))) {

    row <- strategies_df[i, ]
    strat_name <- trimws(row$strategy_name)
    freq       <- trimws(row$rebalance_on)

    # Clean, readable name — no dots, no extra spaces
    clean_name <- paste(strat_name, tools::toTitleCase(freq), sep = " ")

    # Capitalize first letter of frequency for nicer look
    # Alternative: just use freq as-is → "Equal Weight monthly"

    weights <- as.numeric(row[weight_cols])

    port_ret <- tryCatch(
      Return.portfolio(
        R           = returns_xts,
        weights     = weights,
        rebalance_on = freq,
        verbose     = FALSE,
        ...
      ),
      error = function(e) {
        message("Error in '", clean_name, "': ", e$message)
        NULL
      }
    )

    if (!is.null(port_ret)) {
      colnames(port_ret) <- clean_name
      results[[clean_name]] <- port_ret
    }
  }

  if (length(results) == 0) {
    warning("No portfolios were successfully calculated")
    return(NULL)
  }

  # Merge all results into one xts with clean column names
  do.call(merge, results)
}
