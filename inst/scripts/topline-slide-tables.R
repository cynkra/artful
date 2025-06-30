library(tidyverse)
pkgload::load_all()

# ---- Slide 8 -----------------------------------------------------------------
# rt-ds-pretrt.rtf
# rt-ds-trtwk16.rtf

# ---- Slide 9 -----------------------------------------------------------------
# rt-ef-acr20.rtf
# rt-ef-aacr50.rtf
# rt-ef-aacr70.rtf

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
# 6. Update separate_bign() to handle these two cases. Currently it looks for
#    N in brackets, here there are no brackets.

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

# ---- Stats parser ----
# A workflow for parsing stats is seen below. The key mechanism is to
# pivot_longer all the places a stat can be found, and incrementally parse
# these, then pivot_wider again.

df <- bind_rows(one, two)
df_id <- mutate(df, id = row_number(), .before = 1)

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

all_terms <- c(stat_lookup$stat_name, stat_lookup$stat_label)

pattern <- paste0(
  "\\b(",
  paste(str_to_lower(all_terms), collapse = "|"),
  ")\\b"
)

df_id_long <- df_id |>
  pivot_longer(
    starts_with("variable_")
  )

# this isn't working properly, it is matching on values it should not such as
# BASELINE BSA n(%)
stats_found <- df_id_long |>
  filter(
    str_detect(
      str_to_lower(value),
      regex(pattern, ignore_case = TRUE)
    )
  )

stats_unfound <- df_id_long |>
  anti_join(stats_found)

stats_found_parsed <- stats_found |>
  mutate(
    N = if_else(value == "N", stat, NA),
    mean = if_else(value == "MEAN", stat, NA),
    sd = if_else(value == "SD", stat, NA),
    median = if_else(value == "MEDIAN", stat, NA),
    p25 = if_else(
      str_detect(value, "^Q1"),
      str_extract(stat, "^[^,]+"),
      NA
    ),
    p75 = if_else(
      str_detect(value, "Q3$"),
      str_extract(stat, "(?<=,)\\s*([^\\s]+)"),
      NA
    )
  ) |>
  select(-stat) |>
  pivot_longer(
    cols = c("N", "mean", "sd", "median", "p25", "p75"),
    names_to = "stat_name",
    values_to = "stat"
  ) |>
  mutate(stat = str_trim(stat)) |>
  filter(!is.na(stat)) |>
  left_join(stat_lookup) |>
  relocate(stat_label, .before = "stat")

stats_found_parsed_wide <- stats_found_parsed |>
  pivot_wider()

stats_unfound_parsed_wide <- stats_unfound |>
  pivot_wider()

bind_rows(stats_found_parsed_wide, stats_unfound_parsed_wide) |>
  arrange(id)

# TODO:
# - Remove the stats_unfound call, and just expand the block that parses the
#   stats to try and find all stats at once.

# NOTE: I'm not sure this strategy is going to work, it assumes the stat
# is uniquely contained in a single row, and this isn't always the case?
# At least we can't drop any values when pivotting back wider, as we need to
# keep all combinations of variable_* values to identify a single stat.
