# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

**artful** is an experimental R package that converts RTF (Rich Text Format) tables into R data frames following the Analysis Results Data (ARD) standard. The package is designed to parse clinical trial tables typically used in pharmaceutical research.

## Key Commands

### Testing
```bash
# Run all tests
Rscript -e "devtools::test()"

# Run specific test file
Rscript -e "testthat::test_file('tests/testthat/test-qc.R')"
```

### Building and Documentation
```bash
# Build package
Rscript -e "devtools::build()"

# Generate documentation
Rscript -e "devtools::document()"

# Check package
Rscript -e "devtools::check()"
```

### Installation and Loading
```bash
# Install package locally
Rscript -e "devtools::install()"

# Load package
Rscript -e "library(artful)"
```

## Architecture

### Core Processing Pipeline
The main conversion pipeline follows this sequence:
1. **RTF → HTML**: Use Pandoc to convert RTF to HTML (`convert.R:rtf_to_html()`)
2. **HTML → DataFrame**: Extract HTML table using rvest (`convert.R:html_to_dataframe()`)
3. **Preprocessing**: Fix RTF-specific issues (indentation, line breaks) before conversion
4. **Parsing**: Apply heuristics to convert to ARD format (`parse.R`)

### Key Functions
- `rtf_to_ard()` (convert.R:66): Main entry point that orchestrates the full conversion pipeline
- `rtf_to_pdf()` (qc.R): Converts RTF files to PDF using LibreOffice for quality control
- `strip_pagination()` (parse.R:161): Removes headers, footers, and pagination artifacts
- `strip_indentation()` (parse.R:219): Converts RTF indentation to ARD variable columns
- `pivot_group()` (parse.R:287): Transforms wide format grouping variables to long format

### File Structure
- `R/convert.R`: Core RTF→HTML→DataFrame conversion functions
- `R/parse.R`: Table parsing heuristics and ARD transformation
- `R/rtf.R`: RTF-specific text processing (indentation, line breaks)
- `R/qc.R`: Quality control functions (RTF to PDF conversion)
- `R/utils.R`: Utility functions
- `inst/extdata/examples/`: Example RTF files for testing
- `inst/parser-reflections.md`: Technical notes on RTF parsing challenges

### Dependencies
- **Pandoc**: Required system dependency for RTF to HTML conversion
- **LibreOffice**: Required for RTF to PDF conversion (quality control)
- **R packages**: dplyr, purrr, readr, rvest, stringr, tidyr

### ARD Standard Implementation
The package converts tables to Analysis Results Data (ARD) format with these key columns:
- `group*` / `group*_level`: Treatment groups
- `variable` / `variable_level`: Analysis variables
- `stat_name` / `stat_label` / `stat`: Statistical measures

### Exception Handling
- `manage_exceptions()` (parse.R:373): Handles specific tables that don't follow general parsing rules
- Known issue: `rt-ae-eair3.rtf` cannot currently be parsed (see test-coverage.R:19)

### Testing Strategy
- `test-coverage.R`: Tests parsing across all example RTF files
- `test-qc.R`: Tests PDF conversion functionality with mocked system calls
- Expected: 1 parsing failure out of all example files

## Development Notes

### RTF Processing Challenges
- Pandoc drops whitespace, indentation, and line break information
- No standard RTF elements for headers/footers - must be inferred
- Tables may span multiple pages with repeated headers/footers
- Indentation patterns: `\li192` (2 spaces), `\li384` (4 spaces), `\li576` (6 spaces)

### Quality Control
Use `rtf_to_pdf()` to generate PDF versions of RTF files for visual comparison and validation of parsing results.

## System prompts
- You are an expert programmer who prefers the tidyverse
- Follow the tidyverse style guide:
  * Spread long function calls across multiple lines.
  * Where needed, always indent function calls with two spaces.
  * Only name arguments that are less commonly used.
  * Always use double quotes for strings.
  * Use the base pipe, `|>`, not the magrittr pipe `%>%`.
- Use the `.by` argument rather than `group_by()`.
  dplyr 1.1.0 introduced per-operation grouping with the `.by` argument.
  e.g., instead of:
  ```
  transactions |>
    group_by(company, year) |>
    mutate(total = sum(revenue))
  ```
  write this:
  ```
  transactions |>
    mutate(
      total = sum(revenue),
      .by = c(company, year)
    )
  ```