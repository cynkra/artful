library(tidyverse)
library(artful)

# options(tibble.print_max = Inf)

example_data <- function(...) {
  system.file("extdata", "examples", ..., package = "artful")
}

# ---- Slide 7 -----------------------------------------------------------------

# ---- rt-dm-demo.rtf ----
rt_dm_demo <- function(input = example_data("rt-dm-demo.rtf")) {
  temp_rtf <- withr::local_tempfile(fileext = ".rtf")

  input |>
    read_file() |>
    artful:::rtf_indentation() |>
    artful:::rtf_linebreaks() |>
    write_file(temp_rtf)

  ard_ish <- artful:::rtf_to_html(temp_rtf) |>
    artful:::html_to_dataframe() |>
    artful:::manage_exceptions() |>
    artful:::strip_pagination() |>
    artful:::strip_indentation() |>
    artful:::pivot_group()

  ard_ish_parsed <- ard_ish |>
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
      .id = row_number(),
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
    left_join(artful:::stat_lookup)

  big_n <- ard_ish_parsed |>
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

  ard <- ard_ish_parsed |>
    mutate(
      group1_level = stringr::str_extract(
        group1_level,
        ".*?\\S+(?=\\s*(?:\\(N = |N = ))"
      )
    )

  bind_rows(big_n, ard) |>
    select(starts_with("group"), starts_with("variable"), starts_with("stat"))
}

# ---- rt-dm-basedz.rtf ----
rt_dm_basedz <- function(input = example_data("rt-dm-basedz.rtf")) {
  temp_rtf <- withr::local_tempfile(fileext = ".rtf")

  input |>
    read_file() |>
    artful:::rtf_indentation() |>
    write_file(temp_rtf)

  ard_ish <- artful:::rtf_to_html(temp_rtf) |>
    artful:::html_to_dataframe() |>
    artful:::manage_exceptions() |>
    artful:::strip_pagination() |>
    artful:::strip_indentation() |>
    artful:::pivot_group()

  ard_ish_parsed <- ard_ish |>
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
      .id = row_number(),
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
    left_join(artful:::stat_lookup)

  big_n <- ard_ish_parsed |>
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

  ard <- ard_ish_parsed |>
    mutate(
      group1_level = stringr::str_extract(
        group1_level,
        ".*?\\S+(?=\\s*(?:\\(N=|N=))"
      )
    )

  bind_rows(big_n, ard) |>
    select(starts_with("group"), starts_with("variable"), starts_with("stat"))
}

# ---- Slide 8 -----------------------------------------------------------------

# ---- rt-ds-pretrt.rtf ----
rt_ds_pretrt <- function(input = example_data("rt-ds-pretrt.rtf")) {
  temp_rtf <- withr::local_tempfile(fileext = ".rtf")

  input |>
    read_file() |>
    artful:::rtf_indentation() |>
    write_file(temp_rtf)

  ard_ish <- artful:::rtf_to_html(temp_rtf) |>
    artful:::html_to_dataframe() |>
    artful:::manage_exceptions() |>
    artful:::strip_pagination() |>
    artful:::strip_indentation() |>
    artful:::pivot_group()

  ard_ish_parsed <- ard_ish |>
    mutate(
      .id = row_number(),
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
    left_join(artful:::stat_lookup)

  big_n <- ard_ish_parsed |>
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

  ard <- ard_ish_parsed |>
    mutate(
      group1_level = stringr::str_extract(
        group1_level,
        ".*?\\S+(?=\\s*(?:\\(N = |N = ))"
      )
    )

  bind_rows(big_n, ard) |>
    select(starts_with("group"), starts_with("variable"), starts_with("stat"))
}

# ---- rt-ds-trtwk16.rtf ----
rt_ds_trtwk16 <- function(input = example_data("rt-ds-trtwk16.rtf")) {
  temp_rtf <- withr::local_tempfile(fileext = ".rtf")

  input |>
    read_file() |>
    artful:::rtf_indentation() |>
    write_file(temp_rtf)

  ard_ish <- artful:::rtf_to_html(temp_rtf) |>
    artful:::html_to_dataframe() |>
    artful:::manage_exceptions() |>
    artful:::strip_pagination() |>
    artful:::strip_indentation() |>
    artful:::pivot_group()

  ard_ish_parsed <- ard_ish |>
    mutate(
      .id = row_number(),
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
    left_join(artful:::stat_lookup)

  big_n <- ard_ish_parsed |>
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

  ard <- ard_ish_parsed |>
    mutate(
      group1_level = stringr::str_extract(
        group1_level,
        ".*?\\S+(?=\\s*(?:\\(N = |N = ))"
      )
    )

  bind_rows(big_n, ard) |>
    select(starts_with("group"), starts_with("variable"), starts_with("stat"))
}

# ---- Slide 9 -----------------------------------------------------------------

# ---- rt-ef-acr20.rtf ----
rt_ef_acr20 <- function(input = example_data("rt-ef-acr20.rtf")) {
  temp_rtf <- withr::local_tempfile(fileext = ".rtf")

  input |>
    read_file() |>
    artful:::rtf_indentation() |>
    artful:::rtf_linebreaks() |>
    write_file(temp_rtf)

  ard_ish <- artful:::rtf_to_html(temp_rtf) |>
    artful:::html_to_dataframe() |>
    artful:::manage_exceptions() |>
    artful:::strip_pagination() |>
    artful:::strip_indentation() |>
    artful:::pivot_group()

  ard_ish_parsed <- ard_ish |>
    slice(-1:-2) |>
    mutate(stat = if_else(stat == "N.A.", NA, stat)) |>
    mutate(
      .id = row_number(),
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
    left_join(artful:::stat_lookup)

  big_n <- ard_ish_parsed |>
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

  ard <- ard_ish_parsed |>
    mutate(
      group1_level = stringr::str_extract(
        group1_level,
        ".*?\\S+(?=\\s*(?:\\(N = |N = ))"
      )
    )

  bind_rows(big_n, ard) |>
    select(starts_with("group"), starts_with("variable"), starts_with("stat"))
}

# ---- rt-ef-aacr50.rtf ----
rt_ef_aacr50 <- function(input = example_data("rt-ef-aacr50.rtf")) {
  temp_rtf <- withr::local_tempfile(fileext = ".rtf")

  input |>
    read_file() |>
    artful:::rtf_indentation() |>
    artful:::rtf_linebreaks() |>
    write_file(temp_rtf)

  ard_ish <- artful:::rtf_to_html(temp_rtf) |>
    artful:::html_to_dataframe() |>
    artful:::manage_exceptions() |>
    artful:::strip_pagination() |>
    artful:::strip_indentation() |>
    artful:::pivot_group()

  ard_ish_parsed <- ard_ish |>
    filter(variable_label1 != "TOTAL NUMBER OF SUBJECTS") |>
    mutate(stat = if_else(stat == "N.A.", NA, stat)) |>
    mutate(
      .id = row_number(),
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
    left_join(artful:::stat_lookup) |>
    filter(!is.na(stat))

  big_n <- ard_ish_parsed |>
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

  ard <- ard_ish_parsed |>
    mutate(
      group1_level = stringr::str_extract(
        group1_level,
        ".*?\\S+(?=\\s*(?:\\(N = |N = ))"
      )
    )

  bind_rows(big_n, ard) |>
    select(starts_with("group"), starts_with("variable"), starts_with("stat"))
}

# ---- rt-ef-aacr70.rtf ----
rt_ef_aacr70 <- function(input = example_data("rt-ef-aacr70.rtf")) {
  temp_rtf <- withr::local_tempfile(fileext = ".rtf")

  input |>
    read_file() |>
    artful:::rtf_indentation() |>
    artful:::rtf_linebreaks() |>
    write_file(temp_rtf)

  ard_ish <- artful:::rtf_to_html(temp_rtf) |>
    artful:::html_to_dataframe() |>
    artful:::manage_exceptions() |>
    artful:::strip_pagination() |>
    artful:::strip_indentation() |>
    artful:::pivot_group()

  ard_ish_parsed <- ard_ish |>
    filter(variable_label1 != "TOTAL NUMBER OF SUBJECTS") |>
    mutate(stat = if_else(stat == "N.A.", NA, stat)) |>
    filter(!str_detect(stat, "N.E.")) |>
    mutate(
      .id = row_number(),
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
    left_join(artful:::stat_lookup) |>
    filter(!is.na(stat))

  big_n <- ard_ish_parsed |>
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

  ard <- ard_ish_parsed |>
    mutate(
      group1_level = stringr::str_extract(
        group1_level,
        ".*?\\S+(?=\\s*(?:\\(N = |N = ))"
      )
    )

  bind_rows(big_n, ard) |>
    select(starts_with("group"), starts_with("variable"), starts_with("stat"))
}

# ---- Slide 10 ----------------------------------------------------------------

# ---- rt-ef-pasi ----
rt_ef_pasi <- function(input = example_data("rt-ef-pasi.rtf")) {
  temp_rtf <- withr::local_tempfile(fileext = ".rtf")

  input |>
    read_file() |>
    artful:::rtf_indentation() |>
    artful:::rtf_linebreaks() |>
    write_file(temp_rtf)

  ard_ish <- artful:::rtf_to_html(temp_rtf) |>
    artful:::html_to_dataframe() |>
    artful:::manage_exceptions() |>
    artful:::strip_pagination() |>
    artful:::strip_indentation() |>
    artful:::pivot_group()

  ard_ish_parsed <- ard_ish |>
    mutate(stat = if_else(stat == "N.A.", NA, stat)) |>
    mutate(
      .id = row_number(),
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
        variable_level == "TOTAL NUMBER OF SUBJECTS" ~ "n",
        variable_label1 == "RESPONSE RATE (%)" ~ "p",
        variable_label1 == "DIFFERENCE VS PLACEBO (%)" ~ "p",
        variable_level == "P-VALUE" & !is.na(stat) ~ "p_value",
        variable_level == "NON RESPONDERS DUE TO MISSING DATA n (%)" &
          variable_label1 == "RESPONSE RATE (%)" ~
          "p",
        variable_label1 == "RESPONDERS n (%)" & stat == "0" ~ "n",
        .default = NA
      ),
      stat = stat_list,
      .by = .id
    ) |>
    select(
      -c(.id, stat_list, n_values)
    ) |>
    left_join(artful:::stat_lookup) |>
    filter(!is.na(stat)) |>
    mutate(
      stat_name = if_else(
        is.na(variable_level) & variable_label1 == "ODDS RATIO VS PLACEBO",
        "odds_ratio",
        stat_name
      ),
      stat_label = if_else(
        is.na(variable_level) & variable_label1 == "ODDS RATIO VS PLACEBO",
        "Odds Ratio",
        stat_label
      )
    )

  big_n <- ard_ish_parsed |>
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

  ard <- ard_ish_parsed |>
    mutate(
      group1_level = stringr::str_extract(
        group1_level,
        ".*?\\S+(?=\\s*(?:\\(N = |N = ))"
      )
    )

  bind_rows(big_n, ard) |>
    select(starts_with("group"), starts_with("variable"), starts_with("stat"))
}

# ---- rt-ef-enth ----
rt_ef_enth <- function(input = example_data("rt-ef-enth.rtf")) {
  temp_rtf <- withr::local_tempfile(fileext = ".rtf")

  input |>
    read_file() |>
    artful:::rtf_indentation() |>
    artful:::rtf_linebreaks() |>
    write_file(temp_rtf)

  ard_ish <- artful:::rtf_to_html(temp_rtf) |>
    artful:::html_to_dataframe() |>
    artful:::manage_exceptions() |>
    artful:::strip_pagination() |>
    artful:::strip_indentation() |>
    artful:::pivot_group()

  ard_ish_parsed <- ard_ish |>
    mutate(stat = if_else(stat == "N.A.", NA, stat)) |>
    mutate(
      .id = row_number(),
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
        variable_level == "TOTAL NUMBER OF SUBJECTS" ~ "n",
        variable_label1 == "RESPONSE RATE (%)" ~ "p",
        variable_label1 == "DIFFERENCE VS PLACEBO (%)" ~ "p",
        variable_level == "P-VALUE" & !is.na(stat) ~ "p_value",
        variable_level == "NON RESPONDERS DUE TO MISSING DATA n (%)" &
          variable_label1 == "RESPONSE RATE (%)" ~
          "p",
        variable_label1 == "RESPONDERS n (%)" & stat == "0" ~ "n",
        .default = NA
      ),
      stat = stat_list,
      .by = .id
    ) |>
    select(
      -c(.id, stat_list, n_values)
    ) |>
    left_join(artful:::stat_lookup) |>
    filter(!is.na(stat)) |>
    mutate(
      stat_name = if_else(
        is.na(variable_level) & variable_label1 == "ODDS RATIO VS PLACEBO",
        "odds_ratio",
        stat_name
      ),
      stat_label = if_else(
        is.na(variable_level) & variable_label1 == "ODDS RATIO VS PLACEBO",
        "Odds Ratio",
        stat_label
      )
    )

  big_n <- ard_ish_parsed |>
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

  ard <- ard_ish_parsed |>
    mutate(
      group1_level = stringr::str_extract(
        group1_level,
        ".*?\\S+(?=\\s*(?:\\(N = |N = ))"
      )
    )

  bind_rows(big_n, ard) |>
    select(starts_with("group"), starts_with("variable"), starts_with("stat"))
}

# ---- rt-ef-dact ----
rt_ef_dact <- function(input = example_data("rt-ef-dact.rtf")) {
  temp_rtf <- withr::local_tempfile(fileext = ".rtf")

  input |>
    read_file() |>
    artful:::rtf_indentation() |>
    artful:::rtf_linebreaks() |>
    write_file(temp_rtf)

  ard_ish <- artful:::rtf_to_html(temp_rtf) |>
    artful:::html_to_dataframe() |>
    artful:::manage_exceptions() |>
    artful:::strip_pagination() |>
    artful:::strip_indentation() |>
    artful:::pivot_group()

  ard_ish_parsed <- ard_ish |>
    mutate(stat = if_else(stat == "N.A.", NA, stat)) |>
    mutate(
      .id = row_number(),
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
        variable_level == "TOTAL NUMBER OF SUBJECTS" ~ "n",
        variable_label1 == "RESPONSE RATE (%)" ~ "p",
        variable_label1 == "DIFFERENCE VS PLACEBO (%)" ~ "p",
        variable_level == "P-VALUE" & !is.na(stat) ~ "p_value",
        variable_level == "NON RESPONDERS DUE TO MISSING DATA n (%)" &
          variable_label1 == "RESPONSE RATE (%)" ~
          "p",
        variable_label1 == "RESPONDERS n (%)" & stat == "0" ~ "n",
        .default = NA
      ),
      stat = stat_list,
      .by = .id
    ) |>
    select(
      -c(.id, stat_list, n_values)
    ) |>
    left_join(artful:::stat_lookup) |>
    filter(!is.na(stat)) |>
    mutate(
      stat_name = if_else(
        is.na(variable_level) & variable_label1 == "ODDS RATIO VS PLACEBO",
        "odds_ratio",
        stat_name
      ),
      stat_label = if_else(
        is.na(variable_level) & variable_label1 == "ODDS RATIO VS PLACEBO",
        "Odds Ratio",
        stat_label
      )
    )

  big_n <- ard_ish_parsed |>
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

  ard <- ard_ish_parsed |>
    mutate(
      group1_level = stringr::str_extract(
        group1_level,
        ".*?\\S+(?=\\s*(?:\\(N = |N = ))"
      )
    )

  bind_rows(big_n, ard) |>
    select(starts_with("group"), starts_with("variable"), starts_with("stat"))
}

# ---- Slide 11 ----------------------------------------------------------------

# ---- rt-ef-mda ----
rt_ef_mda <- function(input = example_data("rt-ef-mda.rtf")) {
  temp_rtf <- withr::local_tempfile(fileext = ".rtf")

  input |>
    read_file() |>
    artful:::rtf_indentation() |>
    artful:::rtf_linebreaks() |>
    write_file(temp_rtf)

  ard_ish <- artful:::rtf_to_html(temp_rtf) |>
    artful:::html_to_dataframe() |>
    artful:::manage_exceptions() |>
    artful:::strip_pagination() |>
    artful:::strip_indentation() |>
    artful:::pivot_group()

  ard_ish_parsed <- ard_ish |>
    mutate(stat = if_else(stat == "N.A.", NA, stat)) |>
    mutate(
      .id = row_number(),
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
        variable_level == "TOTAL NUMBER OF SUBJECTS" ~ "n",
        variable_label1 == "RESPONSE RATE (%)" ~ "p",
        variable_label1 == "DIFFERENCE VS PLACEBO (%)" ~ "p",
        variable_level == "P-VALUE" & !is.na(stat) ~ "p_value",
        variable_level == "NON RESPONDERS DUE TO MISSING DATA n (%)" &
          variable_label1 == "RESPONSE RATE (%)" ~
          "p",
        variable_label1 == "RESPONDERS n (%)" & stat == "0" ~ "n",
        .default = NA
      ),
      stat = stat_list,
      .by = .id
    ) |>
    select(
      -c(.id, stat_list, n_values)
    ) |>
    left_join(artful:::stat_lookup) |>
    filter(!is.na(stat)) |>
    mutate(
      stat_name = if_else(
        is.na(variable_level) & variable_label1 == "ODDS RATIO VS PLACEBO",
        "odds_ratio",
        stat_name
      ),
      stat_label = if_else(
        is.na(variable_level) & variable_label1 == "ODDS RATIO VS PLACEBO",
        "Odds Ratio",
        stat_label
      )
    )

  big_n <- ard_ish_parsed |>
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

  ard <- ard_ish_parsed |>
    mutate(
      group1_level = stringr::str_extract(
        group1_level,
        ".*?\\S+(?=\\s*(?:\\(N = |N = ))"
      )
    )

  bind_rows(big_n, ard) |>
    select(starts_with("group"), starts_with("variable"), starts_with("stat"))
}

# ---- rt-ef-cfbdas ----
rt_ef_cfbdas <- function(input = example_data("rt-ef-cfbdas.rtf")) {
  temp_rt <- withr::local_tempfile(fileext = ".rtf")

  input |>
    read_file() |>
    artful:::rtf_indentation() |>
    artful:::rtf_linebreaks() |>
    write_file(temp_rt)

  ard_ish <- artful:::rtf_to_html(temp_rt) |>
    artful:::html_to_dataframe() |>
    artful:::manage_exceptions() |>
    artful:::strip_pagination() |>
    artful:::strip_indentation() |>
    artful:::pivot_group()

  ard_ish_parsed <- ard_ish |>
    mutate(stat = if_else(stat == "N.A.", NA, stat)) |>
    mutate(
      .id = row_number(),
      stat_list = str_extract_all(stat, "[\\d.]+")
    ) |>
    mutate(
      n_values = lengths(stat_list)
    ) |>
    unnest(stat_list) |>
    filter(!is.na(stat)) |>
    mutate(
      stat_name = case_when(
        variable_level == "MEAN (SD)" &
          n_values == 2 &
          row_number() == 1 ~
          "mean",
        variable_level == "MEAN (SD)" &
          n_values == 2 &
          row_number() == 2 ~
          "sd",
        variable_level == "MIN, MAX" &
          n_values == 2 &
          row_number() == 1 ~
          "min",
        variable_level == "MIN, MAX" &
          n_values == 2 &
          row_number() == 2 ~
          "max",
        variable_level == "ADJUSTED MEAN DIFFERENCE (SE)" &
          n_values == 2 &
          row_number() == 1 ~
          "mean",
        variable_level == "ADJUSTED MEAN DIFFERENCE (SE)" &
          n_values == 2 &
          row_number() == 2 ~
          "se",
        variable_label1 == "ADJUSTED MEAN CHANGES(SE)" &
          n_values == 2 &
          row_number() == 1 ~
          "mean",
        variable_label1 == "ADJUSTED MEAN CHANGES(SE)" &
          n_values == 2 &
          row_number() == 2 ~
          "se",
        variable_level == "95% CONFIDENCE INTERVAL FOR DIFFERENCE" &
          n_values == 2 &
          row_number() == 1 ~
          "ci_low",
        variable_level == "95% CONFIDENCE INTERVAL FOR DIFFERENCE" &
          n_values == 2 &
          row_number() == 2 ~
          "ci_high",
        variable_label1 == "95% CONFIDENCE INTERVAL FOR MEAN" &
          n_values == 2 &
          row_number() == 1 ~
          "ci_low",
        variable_label1 == "95% CONFIDENCE INTERVAL FOR MEAN" &
          n_values == 2 &
          row_number() == 2 ~
          "ci_high",
        variable_level == "N1" ~ "n",
        variable_level == "MEDIAN" ~ "median",
        variable_level == "P VALUE" ~ "p_value"
      ),
      stat = stat_list,
      .by = .id
    ) |>
    select(
      -c(.id, stat_list, n_values)
    ) |>
    left_join(artful:::stat_lookup)

  big_n <- ard_ish_parsed |>
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

  ard <- ard_ish_parsed |>
    mutate(
      group1_level = stringr::str_extract(
        group1_level,
        ".*?\\S+(?=\\s*(?:\\(N = |N = ))"
      )
    )

  bind_rows(big_n, ard) |>
    select(starts_with("group"), starts_with("variable"), starts_with("stat"))
}

# ---- Slide 12 ----------------------------------------------------------------

# ---- rt-ef-cfbsvdh ----
rt_ef_cfbsvdh <- function(input = example_data("rt-ef-cfbsvdh.rtf")) {
  temp_rtf <- withr::local_tempfile(fileext = ".rtf")

  input |>
    read_file() |>
    artful:::rtf_indentation() |>
    artful:::rtf_linebreaks() |>
    write_file(temp_rtf)

  ard_ish <- artful:::rtf_to_html(temp_rtf) |>
    artful:::html_to_dataframe() |>
    artful:::manage_exceptions() |>
    artful:::strip_pagination() |>
    artful:::strip_indentation() |>
    artful:::pivot_group()

  ard_ish_parsed <- ard_ish |>
    mutate(stat = if_else(stat == "N.A.", NA, stat)) |>
    mutate(
      .id = row_number(),
      stat_list = str_extract_all(stat, "[\\d.]+")
    ) |>
    mutate(
      n_values = lengths(stat_list)
    ) |>
    unnest(stat_list) |>
    filter(!is.na(stat)) |>
    mutate(
      stat_name = case_when(
        variable_level == "MEAN (SD)" &
          n_values == 2 &
          row_number() == 1 ~
          "mean",
        variable_level == "MEAN (SD)" &
          n_values == 2 &
          row_number() == 2 ~
          "sd",
        variable_level == "MIN, MAX" &
          n_values == 2 &
          row_number() == 1 ~
          "min",
        variable_level == "MIN, MAX" &
          n_values == 2 &
          row_number() == 2 ~
          "max",
        variable_level == "ADJUSTED MEAN DIFFERENCE (SE)" &
          n_values == 2 &
          row_number() == 1 ~
          "mean",
        variable_level == "ADJUSTED MEAN DIFFERENCE (SE)" &
          n_values == 2 &
          row_number() == 2 ~
          "se",
        variable_label1 == "ADJUSTED MEAN CHANGES(SE)" &
          n_values == 2 &
          row_number() == 1 ~
          "mean",
        variable_label1 == "ADJUSTED MEAN CHANGES(SE)" &
          n_values == 2 &
          row_number() == 2 ~
          "se",
        variable_level == "95% CONFIDENCE INTERVAL FOR DIFFERENCE" &
          n_values == 2 &
          row_number() == 1 ~
          "ci_low",
        variable_level == "95% CONFIDENCE INTERVAL FOR DIFFERENCE" &
          n_values == 2 &
          row_number() == 2 ~
          "ci_high",
        variable_label1 == "95% CONFIDENCE INTERVAL FOR MEAN" &
          n_values == 2 &
          row_number() == 1 ~
          "ci_low",
        variable_label1 == "95% CONFIDENCE INTERVAL FOR MEAN" &
          n_values == 2 &
          row_number() == 2 ~
          "ci_high",
        variable_level == "N1" ~ "n",
        variable_level == "MEDIAN" ~ "median",
        variable_level == "P VALUE" ~ "p_value"
      ),
      stat = stat_list,
      .by = .id
    ) |>
    select(
      -c(.id, stat_list, n_values)
    ) |>
    left_join(artful:::stat_lookup)

  big_n <- ard_ish_parsed |>
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

  ard <- ard_ish_parsed |>
    mutate(
      group1_level = stringr::str_extract(
        group1_level,
        ".*?\\S+(?=\\s*(?:\\(N = |N = ))"
      )
    )

  bind_rows(big_n, ard) |>
    select(starts_with("group"), starts_with("variable"), starts_with("stat"))
}

# ---- Slide 13 ----------------------------------------------------------------

# ---- rt-ef-cfbhaq ----
rt_ef_cfbhaq <- function(input = example_data("rt-ef-cfbhaq.rtf")) {
  temp_rtf <- withr::local_tempfile(fileext = ".rtf")

  input |>
    read_file() |>
    artful:::rtf_indentation() |>
    artful:::rtf_linebreaks() |>
    write_file(temp_rtf)

  ard_ish <- artful:::rtf_to_html(temp_rtf) |>
    artful:::html_to_dataframe() |>
    artful:::manage_exceptions() |>
    artful:::strip_pagination() |>
    artful:::strip_indentation() |>
    artful:::pivot_group()

  ard_ish_parsed <- ard_ish |>
    mutate(stat = if_else(stat == "N.A.", NA, stat)) |>
    mutate(
      .id = row_number(),
      stat_list = str_extract_all(stat, "[\\d.]+")
    ) |>
    mutate(
      n_values = lengths(stat_list)
    ) |>
    unnest(stat_list) |>
    filter(!is.na(stat)) |>
    mutate(
      stat_name = case_when(
        variable_level == "MEAN (SD)" &
          n_values == 2 &
          row_number() == 1 ~
          "mean",
        variable_level == "MEAN (SD)" &
          n_values == 2 &
          row_number() == 2 ~
          "sd",
        variable_level == "MIN, MAX" &
          n_values == 2 &
          row_number() == 1 ~
          "min",
        variable_level == "MIN, MAX" &
          n_values == 2 &
          row_number() == 2 ~
          "max",
        variable_level == "ADJUSTED MEAN DIFFERENCE (SE)" &
          n_values == 2 &
          row_number() == 1 ~
          "mean",
        variable_level == "ADJUSTED MEAN DIFFERENCE (SE)" &
          n_values == 2 &
          row_number() == 2 ~
          "se",
        variable_label1 == "ADJUSTED MEAN CHANGES(SE)" &
          n_values == 2 &
          row_number() == 1 ~
          "mean",
        variable_label1 == "ADJUSTED MEAN CHANGES(SE)" &
          n_values == 2 &
          row_number() == 2 ~
          "se",
        variable_level == "95% CONFIDENCE INTERVAL FOR DIFFERENCE" &
          n_values == 2 &
          row_number() == 1 ~
          "ci_low",
        variable_level == "95% CONFIDENCE INTERVAL FOR DIFFERENCE" &
          n_values == 2 &
          row_number() == 2 ~
          "ci_high",
        variable_label1 == "95% CONFIDENCE INTERVAL FOR MEAN" &
          n_values == 2 &
          row_number() == 1 ~
          "ci_low",
        variable_label1 == "95% CONFIDENCE INTERVAL FOR MEAN" &
          n_values == 2 &
          row_number() == 2 ~
          "ci_high",
        variable_level == "N1" ~ "n",
        variable_level == "MEDIAN" ~ "median",
        variable_level == "P VALUE" ~ "p_value"
      ),
      stat = stat_list,
      .by = .id
    ) |>
    select(
      -c(.id, stat_list, n_values)
    ) |>
    left_join(artful:::stat_lookup)

  big_n <- ard_ish_parsed |>
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

  ard <- ard_ish_parsed |>
    mutate(
      group1_level = stringr::str_extract(
        group1_level,
        ".*?\\S+(?=\\s*(?:\\(N = |N = ))"
      )
    )

  bind_rows(big_n, ard) |>
    select(starts_with("group"), starts_with("variable"), starts_with("stat"))
}

# ---- rt-ef-cfbsf36 ----
# RTF NOT SUPPLIED

# ---- rt-ef-cfbfaci ----
# RTF NOT SUPPLIED

# ---- Slide 14 ----------------------------------------------------------------

# ---- rt-ae-ae1 ----
rt_ae_ae1 <- function(input = example_data("rt-ae-ae1.rtf")) {
  temp_rtf <- withr::local_tempfile(fileext = ".rtf")

  input |>
    read_file() |>
    artful:::rtf_indentation() |>
    artful:::rtf_linebreaks() |>
    write_file(temp_rtf)

  ard_ish <- artful:::rtf_to_html(temp_rtf) |>
    artful:::html_to_dataframe() |>
    artful:::manage_exceptions() |>
    artful:::strip_pagination() |>
    artful:::strip_indentation() |>
    artful:::pivot_group()

  ard_ish_parsed <- ard_ish |>
    mutate(
      .id = row_number(),
      stat_list = str_extract_all(stat, "[\\d.]+")
    ) |>
    mutate(
      n_values = lengths(stat_list)
    ) |>
    unnest(stat_list) |>
    mutate(
      stat_name = case_when(
        variable_level != "DEATHs" &
          n_values > 1 &
          row_number() == 1 ~
          "n",
        variable_level != "DEATHs" &
          n_values > 1 &
          row_number() == 2 ~
          "p",
        .default = "n"
      ),
      stat = stat_list,
      .by = .id
    ) |>
    select(
      -c(.id, stat_list, n_values)
    ) |>
    left_join(artful:::stat_lookup)

  big_n <- ard_ish_parsed |>
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

  ard <- ard_ish_parsed |>
    mutate(
      group1_level = stringr::str_extract(
        group1_level,
        ".*?\\S+(?=\\s*(?:\\(N = |N = ))"
      )
    )

  bind_rows(big_n, ard) |>
    select(starts_with("group"), starts_with("variable"), starts_with("stat"))
}

# ---- Slide 15 ----------------------------------------------------------------

# ---- rt-ae-aesoc1 ----
rt_ae_aesoc1 <- function(input = example_data("rt-ae-aesoc1.rtf")) {
  temp_rtf <- withr::local_tempfile(fileext = ".rtf")

  input |>
    read_file() |>
    artful:::rtf_indentation() |>
    artful:::rtf_linebreaks() |>
    write_file(temp_rtf)

  ard_ish <- artful:::rtf_to_html(temp_rtf) |>
    artful:::html_to_dataframe() |>
    artful:::manage_exceptions() |>
    artful:::strip_pagination() |>
    artful:::strip_indentation() |>
    artful:::pivot_group()

  ard_ish_parsed <- ard_ish |>
    mutate(
      .id = row_number(),
      stat_list = str_extract_all(stat, "[\\d.]+")
    ) |>
    mutate(
      n_values = lengths(stat_list)
    ) |>
    unnest(stat_list) |>
    mutate(
      stat_name = case_when(
        stat != "0" &
          n_values > 1 &
          row_number() == 1 ~
          "n",
        variable_level != "DEATHs" &
          n_values > 1 &
          row_number() == 2 ~
          "p",
        .default = "n"
      ),
      stat = stat_list,
      .by = .id
    ) |>
    select(
      -c(.id, stat_list, n_values)
    ) |>
    left_join(artful:::stat_lookup)

  big_n <- ard_ish_parsed |>
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

  ard <- ard_ish_parsed |>
    mutate(
      group1_level = stringr::str_extract(
        group1_level,
        ".*?\\S+(?=\\s*(?:\\(N = |N = ))"
      )
    )

  bind_rows(big_n, ard) |>
    select(starts_with("group"), starts_with("variable"), starts_with("stat"))
}

# ---- Slide 16 & 17 -----------------------------------------------------------

# ---- rt-ae-saesoc1 ----
rt_ae_saesoc1 <- function(input = example_data("rt-ae-saesoc1.rtf")) {
  temp_rtf <- withr::local_tempfile(fileext = ".rtf")

  input |>
    read_file() |>
    artful:::rtf_indentation() |>
    artful:::rtf_linebreaks() |>
    write_file(temp_rtf)

  ard_ish <- artful:::rtf_to_html(temp_rtf) |>
    artful:::html_to_dataframe() |>
    artful:::manage_exceptions() |>
    artful:::strip_pagination() |>
    artful:::strip_indentation() |>
    artful:::pivot_group()

  ard_ish_parsed <- ard_ish |>
    mutate(stat = if_else(stat == "N.A.", NA, stat)) |>
    mutate(
      .id = row_number(),
      stat_list = str_extract_all(stat, "[\\d.]+")
    ) |>
    mutate(
      n_values = lengths(stat_list)
    ) |>
    unnest(stat_list) |>
    filter(!is.na(stat)) |>
    mutate(
      stat_name = case_when(
        variable_level == "RATE DIFFERENCE VS PLACEBO (95% CI)" &
          n_values == 3 &
          row_number() == 1 ~
          "estimate",
        variable_level == "RATE DIFFERENCE VS PLACEBO (95% CI)" &
          n_values == 3 &
          row_number() == 2 ~
          "ci_low",
        variable_level == "RATE DIFFERENCE VS PLACEBO (95% CI)" &
          n_values == 3 &
          row_number() == 3 ~
          "ci_high",
        stat != "0" &
          n_values > 1 &
          row_number() == 1 ~
          "n",
        variable_level != "DEATHs" &
          n_values > 1 &
          row_number() == 2 ~
          "p",
        .default = "n"
      ),
      stat = stat_list,
      .by = .id
    ) |>
    select(
      -c(.id, stat_list, n_values)
    ) |>
    left_join(artful:::stat_lookup)

  big_n <- ard_ish_parsed |>
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

  ard <- ard_ish_parsed |>
    mutate(
      group1_level = stringr::str_extract(
        group1_level,
        ".*?\\S+(?=\\s*(?:\\(N = |N = ))"
      )
    )

  bind_rows(big_n, ard) |>
    select(starts_with("group"), starts_with("variable"), starts_with("stat"))
}

# ---- Slide 18 ----------------------------------------------------------------

# RTF UNIDENTIFIED IN PRESENTER NOTES

# ---- Slide 19 ----------------------------------------------------------------

# ---- rt-ae-aedissoc1 ----
rt_ae_aedissoc1 <- function(input = example_data("rt-ae-aedissoc1.rtf")) {
  temp_rtf <- withr::local_tempfile(fileext = ".rtf")

  input |>
    read_file() |>
    artful:::rtf_indentation() |>
    artful:::rtf_linebreaks() |>
    write_file(temp_rtf)

  ard_ish <- artful:::rtf_to_html(temp_rtf) |>
    artful:::html_to_dataframe() |>
    artful:::manage_exceptions() |>
    artful:::strip_pagination() |>
    artful:::strip_indentation() |>
    artful:::pivot_group()

  ard_ish_parsed <- ard_ish |>
    mutate(stat = if_else(stat == "N.A.", NA, stat)) |>
    mutate(
      .id = row_number(),
      stat_list = str_extract_all(stat, "[\\d.]+")
    ) |>
    mutate(
      n_values = lengths(stat_list)
    ) |>
    unnest(stat_list) |>
    filter(!is.na(stat)) |>
    mutate(
      stat_name = case_when(
        variable_level == "RATE DIFFERENCE VS PLACEBO (95% CI)" &
          n_values == 3 &
          row_number() == 1 ~
          "estimate",
        variable_level == "RATE DIFFERENCE VS PLACEBO (95% CI)" &
          n_values == 3 &
          row_number() == 2 ~
          "ci_low",
        variable_level == "RATE DIFFERENCE VS PLACEBO (95% CI)" &
          n_values == 3 &
          row_number() == 3 ~
          "ci_high",
        stat != "0" &
          n_values > 1 &
          row_number() == 1 ~
          "n",
        variable_level != "DEATHs" &
          n_values > 1 &
          row_number() == 2 ~
          "p",
        .default = "n"
      ),
      stat = stat_list,
      .by = .id
    ) |>
    select(
      -c(.id, stat_list, n_values)
    ) |>
    left_join(artful:::stat_lookup)

  big_n <- ard_ish_parsed |>
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

  ard <- ard_ish_parsed |>
    mutate(
      group1_level = stringr::str_extract(
        group1_level,
        ".*?\\S+(?=\\s*(?:\\(N = |N = ))"
      )
    )

  bind_rows(big_n, ard) |>
    select(starts_with("group"), starts_with("variable"), starts_with("stat"))
}

# ---- Slide 20 ----------------------------------------------------------------

# RTF UNIDENTIFIED IN PRESENTER NOTES

# ---- Slide 21 ----------------------------------------------------------------

# RTF UNIDENTIFIED IN PRESENTER NOTES
