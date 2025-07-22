#' Convert RTF file to PDF
#'
#' Converts an RTF file to PDF using LibreOffice's command-line interface.
#'
#' @param rtf_path Character string. Path to the input RTF file.
#' @param pdf_path Character string or NULL. Path for the output PDF file or
#'   directory. If NULL, the PDF will be created in the same location as the
#'   RTF file with a .pdf extension.
#' @param soffice_path Character string. Path to the LibreOffice executable.
#'   Defaults to the standard macOS LibreOffice installation path.
#'
#' @return No return value. The function is called for its side effect of
#'   creating a PDF file.
#'
#' @examples
#' \dontrun{
#' # Convert RTF to PDF in the same directory
#' rtf_to_pdf(rtf_path = "inst/extdata/examples/rt-ae-aesoc2.rtf")
#'
#' # Convert RTF to PDF in a specific directory
#' rtf_to_pdf(rtf_path = "inst/extdata/examples/rt-ae-aesoc2.rtf", pdf_path = "inst/qc/")
#' }
#'
#' @export
rtf_to_pdf <- function(
  rtf_path,
  pdf_path = NULL,
  soffice_path = "/Applications/LibreOffice.app/Contents/MacOS/soffice"
) {
  if (!file.exists(rtf_path)) {
    stop("RTF file not found: ", rtf_path)
  }

  if (is.null(pdf_path)) {
    pdf_path <- sub("\\.rtf$", ".pdf", rtf_path)
  }

  if (dir.exists(pdf_path) || endsWith(pdf_path, "/")) {
    outdir <- pdf_path
    if (!dir.exists(outdir)) {
      dir.create(outdir, recursive = TRUE)
    }
  } else {
    outdir <- dirname(pdf_path)
  }

  system2(
    soffice_path,
    args = c(
      "--headless",
      "--convert-to",
      "pdf",
      "--outdir",
      outdir,
      shQuote(rtf_path)
    ),
    stdout = FALSE,
    stderr = FALSE
  )
}
