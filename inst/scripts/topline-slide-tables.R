library(tidyverse)
pkgload::load_all()

# ---- Slide 7 -----------------------------------------------------------------
# ---- rt-dm-demo.rtf ----
temp_rtf <- tempfile(fileext = ".rtf")

system.file("extdata", "rt-dm-demo.rtf", package = "artful") |>
  read_file() |>
  rtf_indentation() |>
  rtf_linebreaks() |>
  write_file(temp_rtf)

one <- rtf_to_html(temp_rtf) |>
  html_to_dataframe() |>
  manage_exceptions() |>
  strip_pagination() |>
  strip_indentation() |>
  pivot_group()

# ---- rt-dm-basedz.rtf ----
temp_rtf <- tempfile(fileext = ".rtf")

system.file("extdata", "rt-dm-basedz.rtf", package = "artful") |>
  read_file() |>
  rtf_indentation() |>
  write_file(temp_rtf)

two <- rtf_to_html(temp_rtf) |>
  html_to_dataframe() |>
  manage_exceptions() |>
  strip_pagination() |>
  strip_indentation() |>
  pivot_group()

# ---- Slide 8 -----------------------------------------------------------------
# rt-ds-pretrt.rtf
# rt-ds-trtwk16.rtf

# ---- Slide 9 -----------------------------------------------------------------
# rt-ef-acr20.rtf
# rt-ef-aacr50.rtf
# rt-ef-aacr70.rtf
