library(tidyverse)
pkgload::load_all()

# ---- Separate statistics ----
# Depending upon the table, the information detailing which stats are included
# can be found in one of three places:
# 1. Top-left: the top-left column name or cell
# 2. Column-one: In the cells of rows in column 1
# 3. Grouping-colnames: columns that make up the grouping levels in the call to
#    `pivot_group()`

# Top-left
bms_1 <- system.file("extdata", "bms-1.rtf", package = "artful") |>
  rtf_to_html() |>
  html_to_dataframe() |>
  strip_pagination() |>
  separate_indentation() |>
  pivot_group()
bms_1

# Column-one (after calling separate_indentation() and pivot_group() the
# top-left stat becomes part of column one)
bms_2 <- system.file("extdata", "bms-2.rtf", package = "artful") |>
  rtf_to_html() |>
  html_to_dataframe() |>
  strip_pagination() |>
  separate_indentation() |>
  pivot_group()
bms_2

# Column-one
bms_3 <- system.file("extdata", "bms-3.rtf", package = "artful") |>
  rtf_to_html() |>
  html_to_dataframe() |>
  strip_pagination() |>
  separate_indentation() |>
  pivot_group()
bms_3

# Column-one
bms_4 <- system.file("extdata", "bms-4.rtf", package = "artful") |>
  rtf_to_html() |>
  html_to_dataframe() |>
  strip_pagination() |>
  separate_indentation() |>
  pivot_group()
bms_4

# Grouping-colnames
bms_5 <- system.file("extdata", "bms-5.rtf", package = "artful") |>
  rtf_to_html() |>
  html_to_dataframe() |>
  strip_pagination() |>
  separate_indentation() |>
  pivot_group()
bms_5

# ---- Reflections ----
# - All three types of problem are just the same problem. We need to extract the
#   stats from the stat column. The different lies in where the "key" is telling
#   the extraction engine how to parse the stat column. In "top-left" the key is
#   the top-left cell. In "column-one" the keys lie in the the row values in the
#   `variable_level` column. In "grouping-colnames", the key lies in the
#   `group1_level` column. So the extraction engine should have an argument
#   "key", or something of that nature, which informs the stat column how to get
#   parsed.
# - This key can have implict values (top-left) and explicit values (others).
# - the order of the labels (whether extracted or implicitly known) dictates the
#   order of the values.

# ---- Helper funs ----
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

# TODO:
# Currently this function only works for bms_5 and assumes that the stats
# happen to be in the order of n, p, e. The function needs to utilise this idea
# of a key to determine how to parse the stats.

#' Parse a stat col into separate statistics
stat_col_parse <- function(data) {
  if ("stat" %!in% colnames(data)) {
    stop(
      paste0(
        "A column named 'stat' of type `<chr>` containing statistical values ",
        "must be present in the data."
      )
    )
  }

  if ("group1_level" %!in% colnames(data)) {
    stop(
      paste0(
        "A column named 'group1_level' of type `<chr>` containing statistical ",
        "labels must be present in the data."
      )
    )
  }

  extracted_stats <- data |>
    select(group1_level, stat) |>
    mutate(id = row_number()) |>
    mutate(
      n = str_extract(stat, "^\\d+"),
      p = str_extract(stat, "(?<=\\()\\s*([\\d.]+)\\s*(?=\\))", group = 1),
      e = str_extract(stat, "\\d+$")
    ) |>
    select(-group1_level, -stat)

  data |>
    mutate(id = row_number()) |>
    left_join(extracted_stats, by = "id") |>
    select(-stat, -id) |>
    pivot_longer(
      cols = setdiff(colnames(extracted_stats), "id"),
      names_to = "stat_name",
      values_to = "stat"
    )
}

bms_5 |>
  stat_col_parse()

# ---- Top-left workflows ----
# Left to solve: how does the workflow know to allocate "n" and "p". Need some
# input arg which gets populated by a parser which determines the stats to use.
bms_1 |>
  tidyr::separate_wider_regex(
    cols = stat,
    patterns = c(
      n = "\\d+",
      "\\s*\\(\\s*",
      p = "\\d*\\.?\\d+",
      "\\s*\\)"
    )
  ) |>
  tidyr::pivot_longer(
    cols = c(n, p),
    names_to = "stat_name",
    values_to = "stat"
  ) |>
  mutate(
    stat_label = case_when(
      stat_name == "n" ~ "n",
      stat_name == "p" ~ "%",
      .default = stat_name
    )
  ) |>
  separate_bign()

# ---- Column-one workflows ----
bms_3 |>
  mutate(
    n = case_when(
      str_detect(variable_level, "\\(95% CI\\)") ~
        str_extract(stat, "^-?\\d+\\.?\\d*"),
      str_detect(variable_level, "\\(%\\)") ~
        str_extract(stat, "^-?\\d+\\.?\\d*"),
      TRUE ~ str_extract(stat, "^-?\\d+\\.?\\d*")
    ),
    ci_low = case_when(
      str_detect(variable_level, "\\(95% CI\\)") ~
        str_extract(stat, "\\(-?\\d+\\.?\\d*") |>
          str_remove_all("\\(|\\)")
    ),
    ci_high = case_when(
      str_detect(variable_level, "\\(95% CI\\)") ~
        str_extract(stat, ",\\s*-?\\d+\\.?\\d*\\)") |>
          str_remove_all("\\(|\\)|,")
    ),
    p = case_when(
      str_detect(variable_level, "\\(%\\)") ~
        str_extract(stat, "\\(-?\\d+\\.?\\d*\\)") |>
          str_remove_all("\\(|\\)")
    )
  ) |>
  select(-stat) |>
  tidyr::pivot_longer(
    cols = c(n, ci_low, ci_high, p),
    names_to = "stat_name",
    values_to = "stat"
  ) |>
  mutate(
    stat_label = case_when(
      stat_name == "p" ~ "%",
      .default = stat_name
    ),
    .after = stat_name
  ) |>
  separate_bign()

# ---- Grouping-colnames workflow -----
bms_5 |>
  mutate(
    n = str_extract(stat, "^\\d+"),
    p = str_extract(stat, "(?<=\\()\\s*([\\d.]+)\\s*(?=\\))", group = 1),
    e = str_extract(stat, "\\d+$")
  ) |>
  select(-stat) |>
  pivot_longer(
    cols = c(n, p, e),
    names_to = "stat_name",
    values_to = "stat"
  ) |>
  mutate(
    group1_level = str_remove_all(group1_level, "n\\(%\\)\\s+e$"),
    group1_level = str_squish(group1_level)
  ) |>
  mutate(
    stat_label = case_when(
      stat_name == "p" ~ "%",
      .default = stat_name
    ),
    .after = stat_name
  ) |>
  separate_bign()
