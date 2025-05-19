# ------------------------------------------------------------------------------
# Aim: compare solutions to convert RTF tables to data frames
# ------------------------------------------------------------------------------

# ---- Simple RTF table ----
library(gt)
library(unrtf)
pkgload::load_all()

file_rtf <- tempfile(fileext = ".rtf")

gt(head(penguins)) |>
  tab_header(md("**Title**")) |>
  tab_footnote(md("_footnote 1_<br>footnote 2")) |>
  gtsave(file_rtf)

file_artful <- tempfile(fileext = ".html")
file_unrtf <- tempfile(fileext = ".html")
file_knitr <- knitr::pandoc(file_rtf, "html")

html_artful <- rtf_to_html(file_rtf)
html_unrtf <- unrtf::unrtf(file_rtf, "html")
html_knitr <- readLines(file_knitr)

writeLines(html_artful, file_artful)
writeLines(html_unrtf, file_unrtf)

waldo::compare(html_artful, html_unrtf)
waldo::compare(html_artful, html_knitr)
waldo::compare(html_unrtf, html_knitr)

browseURL(file_artful)
browseURL(file_unrtf)
browseURL(file_knitr)

# Summary
# - Return html tables of differing levels of complexity
# - knitr returns the simplest html structure, followed by artful, then unrtf

# ---- Complex RTF table ----
# Test on an actual complex clinical table
file_rtf_complex <- tempfile(fileext = ".rtf")
path_complex <- system.file("extdata", "complex.rtf", package = "artful")
file.copy(path_complex, file_rtf_complex)

file_artful_complex <- tempfile(fileext = ".html")
file_unrtf_complex <- tempfile(fileext = ".html")
file_knitr_complex <- knitr::pandoc(file_rtf_complex, "html")

html_artful_complex <- rtf_to_html(file_rtf_complex)
html_unrtf_complex <- unrtf::unrtf(file_rtf_complex, "html")
html_knitr_complex <- readLines(file_knitr_complex)

writeLines(html_artful_complex, file_artful_complex)
writeLines(html_unrtf_complex, file_unrtf_complex)

waldo::compare(html_artful_complex, html_unrtf_complex)
waldo::compare(html_artful_complex, html_knitr_complex)
waldo::compare(html_unrtf_complex, html_knitr_complex)

browseURL(file_artful_complex)
browseURL(file_unrtf_complex)
browseURL(file_knitr_complex)

# Summary
# - artful solution is the only one which maintained indentation formatting
