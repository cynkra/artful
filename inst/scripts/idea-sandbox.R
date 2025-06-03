library(tidyverse)
pkgload::load_all()

# Separate statistics
# Depending upon the table, the information detailing which stats are included
# can be found in one of three places:
# 1. The top-left column name or cell (bms-1)
# 2. In the cells of rows in column 1 (bms-3)
# 3. In the column header s (bms-4 and bms-5)
# A mixture (bms-2)

# Strategy:

# Find which of the three stat location types a table falls into
# locate_stats <- function()

# Extract the stats into a separate table
# extract_stats <- function()

# Merge the stats back into the original table
# merge_stats <- function()

# Class 1
bms_1 <- system.file("extdata", "bms-1.rtf", package = "artful") |>
  rtf_to_html() |>
  html_to_dataframe() |>
  strip_pagination()
bms_1

# Class 1 and 2
bms_2 <- system.file("extdata", "bms-2.rtf", package = "artful") |>
  rtf_to_html() |>
  html_to_dataframe() |>
  strip_pagination()
bms_2

# Class 2
bms_3 <- system.file("extdata", "bms-3.rtf", package = "artful") |>
  rtf_to_html() |>
  html_to_dataframe() |>
  strip_pagination()
bms_3

# Class 2
bms_4 <- system.file("extdata", "bms-4.rtf", package = "artful") |>
  rtf_to_html() |>
  html_to_dataframe() |>
  strip_pagination()
bms_4

# Class 3
bms_5 <- system.file("extdata", "bms-5.rtf", package = "artful") |>
  rtf_to_html() |>
  html_to_dataframe() |>
  strip_pagination()
bms_5

check_stat <- function(string) {
  str_detect(string, "\\(\\%\\)|\\(95\\% CI\\)")
}

locate_stats <- function(data) {
  first_colname <- check_stat(colnames(data)[[1]])
  print(paste0("First column name: ", first_colname))

  top_left_cell <- check_stat(data[[1]][1])
  print(paste0("Top left cell: ", top_left_cell))

  column_1_cells <- any(check_stat(data[[1]][-1]))
  print(paste0("Column 1 cells: ", column_1_cells))

  grouping_colnames <- any(check_stat(colnames(data)[-1]))
  print(paste0("Grouping colnames: ", grouping_colnames))
}

locate_stats(bms_1)
locate_stats(bms_2)
locate_stats(bms_3)
locate_stats(bms_4)
locate_stats(bms_5)
