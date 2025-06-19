library(tidyverse)
pkgload::load_all()

# ---- Slide 7 -----------------------------------------------------------------

# ---- rt-dm-demo.rtf ----

# Indentation levels are lost when calling pandoc for both RTF's on slide 7.
# This is because the input tables use a mixture of single spaces, double spaces
# and \li calls to signify indentation in the tables. A solution to replace
# these spaces with HTML non-breaking spaces has been implemented in R/rtf.R.
# This solution now needs testing on these files. Once it is confirmed it works,
# a new algroithm to assign indentation levels (and subsequently create new
# columns) needs implementing. This new strategy should be more robust.

# ---- rt-dm-basedz.rtf ----

# The raw table has misalgined columns which throw the reading off from the
# start. We can shift the column names to fix the issue with a custom function:
shift_col_right <- function(df, shift_row = 2L, delete_col = 2L) {
  pattern_row <- as.character(df[shift_row, ])
  result <- map_dfr(
    .x = 1:nrow(df),
    .f = \(x) {
      current_row <- as.character(df[x, ])
      if (identical(current_row, pattern_row)) {
        shifted <- c("", current_row[-length(current_row)])
      } else {
        shifted <- current_row
      }
      tibble(!!!set_names(shifted[-delete_col], names(df)[-delete_col]))
    }
  )
  return(result)
}

system.file("extdata", "rt-dm-basedz.rtf", package = "artful") |>
  rtf_to_html() |>
  html_to_dataframe() |>
  shift_col_right() |> # Shift columns right
  strip_pagination() |>
  separate_indentation() |>
  pivot_group() |>
  View()

# But, we should instead manipualte the raw RTF file to remove this issue so
# it doesn't persist when making the call to pandoc.

# TODO: summary for slide 7 ----
# 1. Update rtf_indentation to replace number of spaces with same number of
#    &nbsp; ✅
# 2. Fix indentation issue and update parsing algorithm to determine levels
# 3. Fix offset column issue by manipulating raw RTF file
# 4. Write stats parsers that will extract stats from the above two examples

# ---- Slide 8 -----------------------------------------------------------------
# rt-ds-pretrt.rtf
# rt-ds-trtwk16.rtf

# ---- Slide 9 -----------------------------------------------------------------
# rt-ef-acr20.rtf
# rt-ef-aacr50.rtf
# rt-ef-aacr70.rtf
