# artful
a**rtf**ul is an experimental R package to convert RTF tables into R data frames following the [Analysis Results Data (ARD) standard](https://wiki.cdisc.org/pages/viewpage.action?pageId=222298985).

## Heuristics
artful works by first converting RTF tables into HTML tables via [Pandoc](https://pandoc.org/).
Then, [rvest](https://rvest.tidyverse.org/) is used to extract the HTML table into an R data frame.
Next, a set of heuristics is used to convert the R data frame to follow the ARD standard. Below is a record of these heuristics, which informs the parsing rules followed in `R/parse.R`:

1. Titles, subtitles, and footnotes should be stripped (and optionally stored as additional attributes or separate metadata).
2. Paginated tables should be combined into a single table, with repeat headers stripped.
3. Indented columns should be separated out into unique columns.
4. Merged columns (e.g., where a count and percentage are combined into a single string) should be separated out into unique columns.
5. Empty rows (where all cells are empty strings) should be removed.
6. Spanning columns headers should be separated out into grouping columns (i.e., a spanning column header represents a unique grouping identifier for the columns it spans over).

## Coverage
As the RTF tables this package attempts to convert are designed to be human readable, and not machine readable, this package will never be able to guarantee 100% coverage across all tables.
Instead the heuristics described above should continually be updated inline with any changes to the code made to accomodate new RTF tables which did not fit the previous rule set.

## ARD standard
ARD is a standardized, machine-readable format specifically designed for encoding statistical analysis summaries derived from clinical trial data. 
However, the ARD standard is still not concretely defined.
This means it is open to interpretation and has some scope of flexibility in terms of columns to be included.
Approximately defined, an ARD data frame should abide to the following criteria:

1. Each row represents a single statistical value (e.g., a count and a percentage must be separated into unique rows and not share the same cell such as "10 (15%)" as is commonly observed in the RTF tables).
This means ARD data frames are somewhat adjacent to tidy data frames in a long format.

2. Each row can provide the context to uniquely identify the unique statistical result value.
This means the data frame should include at least the follow columns (with recommended column names in brackets):
- group names (`group<N>`)
- group levels (`group<N>_level`)
- variable names (`variable`)
- variable levels (`variable_level`)
- statistical names (`stat_name`)
- statistical label (`stat_label`)
- statistical value (`stat`)

3. To create standardisation with other packages such as [cards](https://insightsengineering.github.io/cards/), the following `stat_name` and `stat_label` values should be used:

| `stat_name` | Typical `stat_label` | Description                                                          |
|-------------|----------------------|----------------------------------------------------------------------|
| `n`         | "n" or "Count"       | Count of subjects/records in a specific category or group.           |
| `N`         | "N" or "Total N"     | Total number of subjects/records in a group, often a denominator.    |
| `N_obs`     | "N Observed"         | Number of non-missing observations.                                  |
| `N_miss`    | "N Missing"          | Number of missing observations.                                      |
| `p`         | "%" or "Percent"     | Proportion (often presented as percentage after multiplying by 100). |
| `pct`       | "%" or "Percent"     | Percentage.                                                          |
| `mean`      | "Mean"               | Arithmetic mean.                                                     |
| `sd`        | "SD" or "Std Dev"    | Standard Deviation.                                                  |
| `se`        | "SE" or "Std Err"    | Standard Error of the Mean.                                          |
| `median`    | "Median"             | Median (50th percentile).                                            |
| `p25`       | "Q1" or "25th Pctl"  | 25th Percentile (First Quartile).                                    |
| `p75`       | "Q3" or "75th Pctl"  | 75th Percentile (Third Quartile).                                    |
| `iqr`       | "IQR"                | Interquartile Range (p75 - p25).                                     |
| `min`       | "Min" or "Minimum"   | Minimum value.                                                       |
| `max`       | "Max" or "Maximum"   | Maximum value.                                                       |
| `range`     | "Range"              | Range (often displayed as "min - max").                              |
| `geom_mean` | "Geometric Mean"     | Geometric mean.                                                      |
| `cv`        | "CV (%)"             | Coefficient of Variation.                                            |