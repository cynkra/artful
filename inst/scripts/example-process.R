pkgload::load_all()

# ---- Double indentation ----
# Calling sub-functions
system.file("extdata", "bms-1.rtf", package = "artful") |>
  rtf_to_html() |>
  html_to_dataframe() |>
  strip_pagination() |>
  separate_indentation() |>
  pivot_group() |>
  separate_bign()

# Calling control function
system.file("extdata", "bms-1.rtf", package = "artful") |>
  rtf_to_ard()

# ---- Single indentation ----
# Calling sub-functions
system.file("extdata", "bms-3.rtf", package = "artful") |>
  rtf_to_html() |>
  html_to_dataframe() |>
  strip_pagination() |>
  separate_indentation() |>
  pivot_group() |>
  separate_bign()

# Calling control function
system.file("extdata", "bms-3.rtf", package = "artful") |>
  rtf_to_ard()
