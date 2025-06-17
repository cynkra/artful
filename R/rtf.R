#' Fix Indentation in RTF Files
#'
#' Processes an RTF (Rich Text Format) file to fix indentation issues by
#' replacing RTF-specific indentation codes and spaces with HTML non-breaking
#' space entities.
#'
#' @param file A character string specifying the path to the RTF file to be
#'   processed. The file should be readable and contain valid RTF content.
#'
#' @return A character string containing the processed RTF content with
#'   indentation codes replaced by HTML non-breaking space entities (`&nbsp;`).
#'   The original file is not modified; only the processed string is returned.
#'
#' @details
#' The function performs the following transformations:
#' \itemize{
#'   \item Replaces spaces immediately after opening braces before `\cell}` with `&nbsp;`
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
#' fix_indentation("path/to/your/file.rtf")
#' }
#'
#' @export
fix_indentation <- function(file) {
  rtf_string <- readr::read_file(file)
  rtf_string |>
    stringr::str_replace_all("(\\{)( +)(.*?\\\\cell\\})", "\\1&nbsp;\\3") |>
    stringr::str_replace_all("\\\\li192", "&nbsp;&nbsp;") |>
    stringr::str_replace_all("\\\\li384", "&nbsp;&nbsp;&nbsp;&nbsp;") |>
    stringr::str_replace_all(
      "\\\\li576",
      "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"
    )
}
