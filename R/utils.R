#' @noRd
'%!in%' <- function(x, y) !('%in%'(x, y))

#' @noRd
prinf <- function(x) print(x, n = Inf)

stat_lookup <- tribble(
  ~stat_name, ~stat_label,
  "n", "n",
  "N", "N",
  "N_obs", "N Observed",
  "N_miss", "N Missing",
  "p", "%" ,
  "pct", "%" ,
  "mean", "Mean" ,
  "sd", "SD",
  "se", "SE",
  "median", "Median" ,
  "p25", "Q1",
  "p75", "Q3",
  "iqr", "IQR" ,
  "min", "Min",
  "max", "Max",
  "range", "Range",
  "geom_mean", "Geometric Mean",
  "cv", "CV (%)",
  "ci_low", "CI Lower Bound",
  "ci_high", "CI Upper Bound",
  "p_value", "p",
  "estimate", "est"
)
