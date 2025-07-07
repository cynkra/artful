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
  "cv", "CV (%)",
  "ci_low", "CI Lower Bound",
  "ci_high", "CI Upper Bound",
  "p_value", "p",
  "estimate", "est"
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
  filter(!is.na(stat)) |>
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

prinf(rt_dm_demo)

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
  filter(!is.na(stat)) |>
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

prinf(rt_dm_basedz)

# ---- Slide 8 -----------------------------------------------------------------
# rt-ds-pretrt.rtf
temp_rtf <- tempfile(fileext = ".rtf")

system.file("extdata", "rt-ds-pretrt.rtf", package = "artful") |>
  read_file() |>
  rtf_indentation() |>
  write_file(temp_rtf)

rt_ds_pretrt <- rtf_to_html(temp_rtf) |>
  html_to_dataframe() |>
  manage_exceptions() |>
  strip_pagination() |>
  strip_indentation() |>
  pivot_group()

# Parse stats
rt_ds_pretrt <- rt_ds_pretrt |>
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
      .default = NA
    ),
    stat = stat_list,
    .by = .id
  ) |>
  select(
    -c(.id, stat_list, n_values)
  ) |>
  filter(!is.na(stat)) |>
  left_join(stat_lookup)

big_n <- rt_ds_pretrt |>
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

rt_ds_pretrt <- rt_ds_pretrt |>
  mutate(
    group1_level = stringr::str_extract(
      group1_level,
      ".*?\\S+(?=\\s*(?:\\(N = |N = ))"
    )
  )

rt_ds_pretrt <- bind_rows(big_n, rt_ds_pretrt) |>
  select(starts_with("group"), starts_with("variable"), starts_with("stat"))

prinf(rt_ds_pretrt)

# rt-ds-trtwk16.rtf
temp_rtf <- tempfile(fileext = ".rtf")

system.file("extdata", "rt-ds-trtwk16.rtf", package = "artful") |>
  read_file() |>
  rtf_indentation() |>
  write_file(temp_rtf)

rt_ds_trtwk16 <- rtf_to_html(temp_rtf) |>
  html_to_dataframe() |>
  manage_exceptions() |>
  strip_pagination() |>
  strip_indentation() |>
  pivot_group()

# Parse stats
rt_ds_trtwk16 <- rt_ds_trtwk16 |>
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
      .default = NA
    ),
    stat = stat_list,
    .by = .id
  ) |>
  select(
    -c(.id, stat_list, n_values)
  ) |>
  mutate(stat_name = if_else(is.na(stat_name), "n", stat_name)) |>
  filter(!is.na(stat)) |>
  left_join(stat_lookup)

big_n <- rt_ds_trtwk16 |>
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

rt_ds_trtwk16 <- rt_ds_trtwk16 |>
  mutate(
    group1_level = stringr::str_extract(
      group1_level,
      ".*?\\S+(?=\\s*(?:\\(N = |N = ))"
    )
  )

rt_ds_trtwk16 <- bind_rows(big_n, rt_ds_trtwk16) |>
  select(starts_with("group"), starts_with("variable"), starts_with("stat"))

prinf(rt_ds_trtwk16)

# ---- Slide 9 -----------------------------------------------------------------
# rt-ef-acr20.rtf
temp_rtf <- tempfile(fileext = ".rtf")

system.file("extdata", "rt-ef-acr20.rtf", package = "artful") |>
  read_file() |>
  rtf_indentation() |>
  rtf_linebreaks() |>
  write_file(temp_rtf)

rt_ef_acr20 <- rtf_to_html(temp_rtf) |>
  html_to_dataframe() |>
  manage_exceptions() |>
  strip_pagination() |>
  strip_indentation() |>
  pivot_group()

# Parse stats
rt_ef_acr20 <- rt_ef_acr20 |>
  slice(-1:-2) |>
  mutate(stat = if_else(stat == "N.A.", NA, stat)) |>
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
      str_detect(variable_level, "n \\(%\\)$") &
        n_values > 1 &
        row_number() == 1 ~
        "n",
      str_detect(variable_level, "n \\(%\\)$") &
        n_values > 1 &
        row_number() == 2 ~
        "p",
      str_detect(variable_level, "\\(95% CI\\)") &
        n_values > 1 &
        row_number() == 1 ~
        "ci_low",
      str_detect(variable_level, "\\(95% CI\\)") &
        n_values > 1 &
        row_number() == 2 ~
        "ci_high",
      variable_level == "P-VALUE" ~ "p_value",
      .default = NA
    ),
    stat = stat_list,
    .by = .id
  ) |>
  select(
    -c(.id, stat_list, n_values)
  ) |>
  filter(!is.na(stat)) |>
  left_join(stat_lookup)

big_n <- rt_ef_acr20 |>
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

rt_ef_acr20 <- rt_ef_acr20 |>
  mutate(
    group1_level = stringr::str_extract(
      group1_level,
      ".*?\\S+(?=\\s*(?:\\(N = |N = ))"
    )
  )

rt_ef_acr20 <- bind_rows(big_n, rt_ef_acr20) |>
  select(starts_with("group"), starts_with("variable"), starts_with("stat"))

prinf(rt_ef_acr20)

# rt-ef-aacr50.rtf
temp_rtf <- tempfile(fileext = ".rtf")

system.file("extdata", "rt-ef-aacr50.rtf", package = "artful") |>
  read_file() |>
  rtf_indentation() |>
  rtf_linebreaks() |>
  write_file(temp_rtf)

rt_ef_aacr50 <- rtf_to_html(temp_rtf) |>
  html_to_dataframe() |>
  manage_exceptions() |>
  strip_pagination() |>
  strip_indentation() |>
  pivot_group()

# Parse stats
rt_ef_aacr50 <- rt_ef_aacr50 |>
  filter(variable_label1 != "TOTAL NUMBER OF SUBJECTS") |>
  mutate(stat = if_else(stat == "N.A.", NA, stat)) |>
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
      str_detect(variable_level, "n \\(%\\)$") &
        n_values > 1 &
        row_number() == 1 ~
        "n",
      str_detect(variable_level, "n \\(%\\)$") &
        n_values > 1 &
        row_number() == 2 ~
        "p",
      str_detect(variable_label1, "n \\(%\\)$") &
        n_values > 1 &
        row_number() == 1 ~
        "n",
      str_detect(variable_label1, "n \\(%\\)$") &
        n_values > 1 &
        row_number() == 2 ~
        "p",
      str_detect(variable_level, "\\(95% CI\\)") &
        n_values > 1 &
        row_number() == 1 ~
        "ci_low",
      str_detect(variable_level, "\\(95% CI\\)") &
        n_values > 1 &
        row_number() == 2 ~
        "ci_high",
      variable_label1 == "P-VALUE" & !is.na(stat) ~ "p_value",
      variable_level == "NON RESPONDERS DUE TO MISSING DATA n (%)" &
        variable_label1 == "RESPONSE RATE (%)" ~
        "p",
      .default = NA
    ),
    stat = stat_list,
    .by = .id
  ) |>
  mutate(
    stat_name = case_when(
      variable_label1 == "DIFFERENCE VS PLACEBO (%)" &
        !is.na(stat) &
        is.na(stat_name) ~
        "p",
      variable_label1 == "ODDS RATIO VS PLACEBO" &
        !is.na(stat) &
        is.na(stat_name) ~
        "estimate",
      .default = stat_name
    ),
  ) |>
  select(
    -c(.id, stat_list, n_values)
  ) |>
  left_join(stat_lookup) |>
  filter(!is.na(stat))

big_n <- rt_ef_aacr50 |>
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

rt_ef_aacr50 <- rt_ef_aacr50 |>
  mutate(
    group1_level = stringr::str_extract(
      group1_level,
      ".*?\\S+(?=\\s*(?:\\(N = |N = ))"
    )
  )

rt_ef_aacr50 <- bind_rows(big_n, rt_ef_aacr50) |>
  select(starts_with("group"), starts_with("variable"), starts_with("stat"))

prinf(rt_ef_aacr50)

# rt-ef-aacr70.rtf
temp_rtf <- tempfile(fileext = ".rtf")

system.file("extdata", "rt-ef-aacr70.rtf", package = "artful") |>
  read_file() |>
  rtf_indentation() |>
  rtf_linebreaks() |>
  write_file(temp_rtf)

rt_ef_aacr70 <- rtf_to_html(temp_rtf) |>
  html_to_dataframe() |>
  manage_exceptions() |>
  strip_pagination() |>
  strip_indentation() |>
  pivot_group()

# Parse stats
rt_ef_aacr70 <- rt_ef_aacr70 |>
  filter(variable_label1 != "TOTAL NUMBER OF SUBJECTS") |>
  mutate(stat = if_else(stat == "N.A.", NA, stat)) |>
  filter(!str_detect(stat, "N.E.")) |>
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
      str_detect(variable_level, "n \\(%\\)$") &
        n_values > 1 &
        row_number() == 1 ~
        "n",
      str_detect(variable_level, "n \\(%\\)$") &
        n_values > 1 &
        row_number() == 2 ~
        "p",
      str_detect(variable_label1, "n \\(%\\)$") &
        n_values > 1 &
        row_number() == 1 ~
        "n",
      str_detect(variable_label1, "n \\(%\\)$") &
        n_values > 1 &
        row_number() == 2 ~
        "p",
      str_detect(variable_level, "\\(95% CI\\)") &
        n_values > 1 &
        row_number() == 1 ~
        "ci_low",
      str_detect(variable_level, "\\(95% CI\\)") &
        n_values > 1 &
        row_number() == 2 ~
        "ci_high",
      variable_label1 == "P-VALUE" & !is.na(stat) ~ "p_value",
      variable_level == "NON RESPONDERS DUE TO MISSING DATA n (%)" &
        variable_label1 == "RESPONSE RATE (%)" ~
        "p",
      variable_label1 == "RESPONDERS n (%)" & stat == "0" ~ "n",
      .default = NA
    ),
    stat = stat_list,
    .by = .id
  ) |>
  mutate(
    stat_name = case_when(
      variable_label1 == "DIFFERENCE VS PLACEBO (%)" &
        !is.na(stat) &
        is.na(stat_name) ~
        "p",
      variable_label1 == "ODDS RATIO VS PLACEBO" &
        !is.na(stat) &
        is.na(stat_name) ~
        "estimate",
      .default = stat_name
    ),
  ) |>
  select(
    -c(.id, stat_list, n_values)
  ) |>
  left_join(stat_lookup) |>
  filter(!is.na(stat))

big_n <- rt_ef_aacr70 |>
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

rt_ef_aacr70 <- rt_ef_aacr70 |>
  mutate(
    group1_level = stringr::str_extract(
      group1_level,
      ".*?\\S+(?=\\s*(?:\\(N = |N = ))"
    )
  )

rt_ef_aacr70 <- bind_rows(big_n, rt_ef_aacr70) |>
  select(starts_with("group"), starts_with("variable"), starts_with("stat"))

prinf(rt_ef_aacr70)
