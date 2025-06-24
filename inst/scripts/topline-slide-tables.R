library(tidyverse)
pkgload::load_all()

# ---- Slide 7 -----------------------------------------------------------------

# ---- rt-dm-demo.rtf ----

# ---- rt-dm-basedz.rtf ----

# But, we should instead manipualte the raw RTF file to remove this issue so
# it doesn't persist when making the call to pandoc.

# TODO: summary for slide 7 ----
# 1. Update rtf_indentation to replace number of spaces with same number of
#    &nbsp; ✅
# 2. Check that rtf_indentation() correctly detects indentation for the tables
#    listed above (render HTML), and tweak function as necessary. Note: the
#    RTF files will first need to be read in and converted to a string before
#    being passed to rtf_indentation(). Use readr::read_file() here. Code:
#
# file_1 <- system.file("extdata", "rt-dm-demo.rtf", package = "artful")
# file_2 <- system.file("extdata", "rt-dm-basedz.rtf", package = "artful")

# temp_rtf <- tempfile(fileext = ".rtf")
# temp_html <- tempfile(fileext = ".html")

# read_file(file_2) |>
#   rtf_indentation() |>
#   write_lines(temp_rtf)

# rtf_to_html(temp_rtf) |>
#   write_lines(temp_html)

# browseURL(temp_html)
#
#    Confirmed that this works BUT indentation is not consistent. It jumps from
#    two spaces, to eight. We need some clever parsing algorithm in 3. ✅
#
# 3. Fix indentation issue and update parsing algorithm to determine levels
# 4. Fix offset column issue by manipulating raw RTF file
# 5. Write stats parsers that will extract stats from the above two examples

# ---- Slide 8 -----------------------------------------------------------------
# rt-ds-pretrt.rtf
# rt-ds-trtwk16.rtf

# ---- Slide 9 -----------------------------------------------------------------
# rt-ef-acr20.rtf
# rt-ef-aacr50.rtf
# rt-ef-aacr70.rtf

library(tidyverse)
pkgload::load_all()

temp_rtf <- tempfile(fileext = ".rtf")
system.file("extdata", "rt-dm-demo.rtf", package = "artful") |>
  # system.file("extdata", "rt-dm-basedz.rtf", package = "artful") |>
  read_file() |>
  rtf_indentation() |>
  write_file(temp_rtf)

# df <-
rtf_to_html(temp_rtf) |>
  html_to_dataframe() |>
  manage_exceptions()

strip_pagination()

df_labelled <- df |>
  mutate(nbsp_count = str_count(df[[1]], fixed("&nbsp;"))) |>
  mutate(group = cumsum(nbsp_count == 0)) |>
  mutate(
    counter = case_when(
      nbsp_count == 0 ~ 0,
      nbsp_count > lag(nbsp_count, default = 0) ~ 1,
      nbsp_count == lag(nbsp_count, default = 0) ~ 0,
      nbsp_count < lag(nbsp_count, default = 0) ~ -1
    )
  ) |>
  mutate(indent_level = cumsum(counter), .by = group) |>
  mutate(
    indent_level_rev = max(indent_level) + min(indent_level) - indent_level,
    .by = group
  ) |>
  rename(value = 1) |>
  mutate(variable_label = paste0("variable_label", indent_level_rev)) |>
  mutate(id = row_number()) |> # prevents `pivot_wider()` errors caused by duplicate values
  pivot_wider(
    names_from = variable_label,
    values_from = value,
    id_cols = everything() # Keep all existing columns for the pivot
  ) |>
  fill(starts_with("variable_label")) |>
  rename(variable_level = variable_label0) |>
  select(
    variable_level,
    starts_with("variable_label"),
    everything(),
    -id,
    -nbsp_count,
    -group,
    -counter,
    -indent_level,
    -indent_level_rev
  ) |>
  mutate(
    across(
      everything(),
      ~ str_replace_all(., fixed("&nbsp;"), "")
    )
  )

df_labelled |>
  pivot_group() |>
  View()
