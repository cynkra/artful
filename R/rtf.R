#' Fix Indentation issues in RTF encoded strings
#'
#' Processes an RTF (Rich Text Format) encoded string to fix indentation issues
#' by replacing RTF-specific indentation codes and spaces with HTML non-breaking
#' space entities.
#'
#' @param string A character vector specifying the RTF encodings.
#'
#' @return A character string containing the processed RTF content with
#'   indentation codes replaced by HTML non-breaking space entities (`&nbsp;`).
#'
#' @details
#' The function performs the following transformations:
#' \itemize{
#'   \item Replaces spaces immediately after opening braces before `\cell}` with
#'     the same number of `&nbsp;` calls.
#'   \item Converts `\li192` codes to two non-breaking spaces
#'   \item Converts `\li384` codes to four non-breaking spaces
#'   \item Converts `\li576` codes to six non-breaking spaces
#' }
#'
#' These transformations correspond to different levels of indentation commonly
#' found in RTF table cells and list items.
#'
#' @examples
#' \dontrun{
#' rtf_indentation("{  two spaces inside the cell\\cell}")
#' rtf_indentation("\\li192{two spaces before the cell\\cell}")
#' rtf_indentation("\\li576{ Seven spaces in total\\cell}")
#' }
#'
#' @keywords internal
rtf_indentation <- function(string) {
  string |>
    str_replace_all("(\\{)( +)(.*?\\\\cell\\})", function(match) {
      groups <- str_match(match, "(\\{)( +)(.*?\\\\cell\\})")
      space_count <- nchar(groups[, 3])
      nbsp_replacement <- paste(rep("&nbsp;", space_count), collapse = "")
      paste0(groups[, 2], nbsp_replacement, groups[, 4])
    }) |>
    str_replace_all("\\\\li192", "&nbsp;&nbsp;") |>
    str_replace_all("\\\\li384", "&nbsp;&nbsp;&nbsp;&nbsp;") |>
    str_replace_all(
      "\\\\li576",
      "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"
    ) |>
    str_replace_all("\\\\pnhang", "&nbsp;&nbsp;")
}

#' Replace line breaks with spaces in RTF encoded strings
#'
#' Processes an RTF (Rich Text Format) encoded string by replacing line breaks,
#' represented as "{\line}", with spaces. This ensures all content in a single
#' cell is represented as a single character vector.
#'
#' @param string A character vector specifying the RTF encodings.
#'
#' @return A character string containing the processed RTF content with line
#' breaks replaced by spaces.
#'
#' @keywords internal
rtf_linebreaks <- function(string) {
  str_replace_all(string, "\\{\\\\line\\}", " ")
}
