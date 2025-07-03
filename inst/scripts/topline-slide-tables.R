library(tidyverse)
pkgload::load_all()

stat_lookup <- tribble(
  ~stat_name, ~stat_label,
  "n", "n",
  "N", "N",
  "N_obs", "N Observed",
  "N_miss", "N Missing", 
  "p", "%" ,
  "pct", "%" ,
  "mean", "Mean" ,
  "sd", "SD",
  "se", "SE", 
  "median", "Median" ,
  "p25", "Q1",
  "p75", "Q3",
  "iqr", "IQR" ,	
  "min", "Min", 
  "max", "Max",
  "range", "Range", 	
  "geom_mean", "Geometric Mean",
  "cv", "CV (%)" 
)

# ---- Slide 7 -----------------------------------------------------------------
# ---- rt-dm-demo.rtf ----
temp_rtf <- tempfile(fileext = ".rtf")

system.file("extdata", "rt-dm-demo.rtf", package = "artful") |>
  read_file() |>
  rtf_indentation() |>
  rtf_linebreaks() |>
  write_file(temp_rtf)

rt_dm_demo <- rtf_to_html(temp_rtf) |>
  html_to_dataframe() |>
  manage_exceptions() |>
  strip_pagination() |>
  strip_indentation() |>
  pivot_group()

# Parse stats
rt_dm_demo <- rt_dm_demo |>
  separate_longer_delim(
    cols = c(variable_level, stat),
    delim = ", "
  ) |>
  mutate(
    stat_name = case_when(
      variable_level == "N" ~ "N",
      variable_level == "MEAN" ~ "mean",
      variable_level == "MEDIAN" ~ "median",
      variable_level == "SD" ~ "sd",
      variable_level == "Q1" ~ "p25",
      variable_level == "Q3" ~ "p75",
      variable_level == "MAX" ~ "min",
      variable_level == "MIN" ~ "max",
      .default = NA
    )
  ) |>
  mutate(
    .id = dplyr::row_number(),
    stat_list = str_extract_all(stat, "[\\d.]+")
  ) |>
  mutate(
    n_values = lengths(stat_list)
  ) |>
  unnest(stat_list) |>
  mutate(
    stat_name = case_when(
      n_values > 1 & row_number() == 1 ~ "n",
      n_values > 1 & row_number() == 2 ~ "p",
      .default = stat_name
    ),
    stat = stat_list,
    .by = .id
  ) |>
  select(
    -c(.id, stat_list, n_values)
  ) |>
  mutate(stat_name = if_else(is.na(stat_name), "n", stat_name)) |>
  left_join(stat_lookup)

big_n <- rt_dm_demo |>
  distinct(group1_level) |>
  separate_wider_regex(
    group1_level,
    patterns = c(
      variable_label1 = ".*?\\S+",
      "\\s+",
      "(?:\\(N = |N = )",
      stat = "\\d+",
      "\\)?"
    )
  ) |>
  mutate(stat_name = "N_header", stat_label = "N", .before = "stat")

rt_dm_demo <- rt_dm_demo |>
  mutate(
    group1_level = stringr::str_extract(
      group1_level,
      ".*?\\S+(?=\\s*(?:\\(N = |N = ))"
    )
  )

rt_dm_demo <- bind_rows(big_n, rt_dm_demo) |>
  select(starts_with("group"), starts_with("variable"), starts_with("stat"))


# ---- rt-dm-basedz.rtf ----
temp_rtf <- tempfile(fileext = ".rtf")

system.file("extdata", "rt-dm-basedz.rtf", package = "artful") |>
  read_file() |>
  rtf_indentation() |>
  write_file(temp_rtf)

rt_dm_basedz <- rtf_to_html(temp_rtf) |>
  html_to_dataframe() |>
  manage_exceptions() |>
  strip_pagination() |>
  strip_indentation() |>
  pivot_group()

# Parse stats
rt_dm_basedz <- rt_dm_basedz |>
  separate_longer_delim(
    cols = c(variable_level, stat),
    delim = ", "
  ) |>
  mutate(
    stat_name = case_when(
      variable_level == "N" ~ "N",
      variable_level == "MEAN" ~ "mean",
      variable_level == "MEDIAN" ~ "median",
      variable_level == "SD" ~ "sd",
      variable_level == "Q1" ~ "p25",
      variable_level == "Q3" ~ "p75",
      variable_level == "MAX" ~ "min",
      variable_level == "MIN" ~ "max",
      .default = NA
    )
  ) |>
  mutate(
    .id = dplyr::row_number(),
    stat_list = str_extract_all(stat, "[\\d.]+")
  ) |>
  mutate(
    n_values = lengths(stat_list)
  ) |>
  unnest(stat_list) |>
  mutate(
    stat_name = case_when(
      n_values > 1 & row_number() == 1 ~ "n",
      n_values > 1 & row_number() == 2 ~ "p",
      .default = stat_name
    ),
    stat = stat_list,
    .by = .id
  ) |>
  select(
    -c(.id, stat_list, n_values)
  ) |>
  mutate(stat_name = if_else(is.na(stat_name), "n", stat_name)) |>
  left_join(stat_lookup)

big_n <- rt_dm_basedz |>
  distinct(group1_level) |>
  separate_wider_regex(
    group1_level,
    patterns = c(
      variable_label1 = ".+?",
      "\\s+(?:\\()?N=",
      stat = "\\d+",
      "\\)?"
    )
  ) |>
  mutate(
    stat_name = "N_header",
    stat_label = "N",
    .before = "stat"
  )

rt_dm_basedz <- rt_dm_basedz |>
  mutate(
    group1_level = stringr::str_extract(
      group1_level,
      ".*?\\S+(?=\\s*(?:\\(N=|N=))"
    )
  )

rt_dm_basedz <- bind_rows(big_n, rt_dm_basedz) |>
  select(starts_with("group"), starts_with("variable"), starts_with("stat"))

# ---- Slide 8 -----------------------------------------------------------------
# rt-ds-pretrt.rtf
# rt-ds-trtwk16.rtf

# ---- Slide 9 -----------------------------------------------------------------
# rt-ef-acr20.rtf
# rt-ef-aacr50.rtf
# rt-ef-aacr70.rtf
