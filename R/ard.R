#' Convert an RTF Table into an ARD data frame
#'
#' This function converts a table in RTF format into a data frame in R,
#' following the ARD standard.
#'
#' @param file A string, the path to the input .rtf file.
#'
#' @return an R data frame following the ARD standard.
#'
#' @export
rtf_to_ard <- function(file) {
  file |>
    rtf_to_html() |>
    html_to_dataframe() |>
    strip_pagination() |>
    separate_indentation() |>
    pivot_group()
}
