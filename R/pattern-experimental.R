#' Try to match a stat string against a regex
#'
#' @param stat Character string to match
#'
#' @return Matching regex
try_regex_match <- function(stats) {
  regex_list <- get_default_patterns() |>
    map("regex") |>
    list_c()

  map_chr(
    stats,
    \(x) {
      detected_pattern <- detect(
        regex_list,
        .f = \(y) str_detect(x, y),
        .default = NA_character_
      )
    }
  )
}
