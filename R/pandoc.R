#' Convert an RTF file to HTML using Pandoc
#'
#' This function takes the path to an input RTF file and uses the Pandoc
#' command-line tool to convert it to HTML.
#'
#' @param file A string, the path to the input .rtf file.
#'
#' @return Character vector of HTML.
#'
#' @export
rtf_to_html <- function(file) {
  if (!file.exists(file)) {
    stop("Input RTF file does not exist: ", file)
  }

  pandoc_path <- Sys.which("pandoc")
  if (pandoc_path == "") {
    stop(
      "Pandoc command not found. ",
      "Please ensure Pandoc is installed and in your system's PATH."
    )
  }

  message("Attempting conversion...")

  result <- system2(
    "pandoc",
    args = c("--from", "rtf", "--to", "html", shQuote(file)),
    stdout = TRUE,
    stderr = TRUE
  )

  status <- attr(result, "status", TRUE)
  if (length(status) > 0 && status > 0) {
    rlang::abort(c("Running Pandoc failed with following error", result))
  }

  return(result)
}
