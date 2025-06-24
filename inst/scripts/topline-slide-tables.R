library(tidyverse)
pkgload::load_all()

# ---- Slide 7 -----------------------------------------------------------------
# TODO:
# 1. Update rtf_indentation to replace number of spaces with same number of
#    &nbsp; ✅
# 2. Check that rtf_indentation() correctly detects indentation for the tables
#    listed above (render HTML), and tweak function as necessary. Note: the
#    RTF files will first need to be read in and converted to a string before
#    being passed to rtf_indentation(). Use readr::read_file() here. ✅
# 3. Fix indentation issue and update parsing algorithm to determine levels ✅
# 4. Fix offset column issue by manipulating raw RTF file ✅
# 5. Write stats parsers that will extract stats from the above two examples

# ---- rt-dm-demo.rtf ----
temp_rtf <- tempfile(fileext = ".rtf")

system.file("extdata", "rt-dm-demo.rtf", package = "artful") |>
  read_file() |>
  rtf_indentation() |>
  write_file(temp_rtf)

rtf_to_html(temp_rtf) |>
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

rtf_to_html(temp_rtf) |>
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
