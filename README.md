# artful
a**rtf**ul is an experimental R package to convert RTF tables into R data frames following the [ARD standard](https://wiki.cdisc.org/pages/viewpage.action?pageId=222298985).

## Heuristics
artful works by first converting RTF tables into HTML tables via [Pandoc](https://pandoc.org/).
Then, [rvest](https://rvest.tidyverse.org/) is used to extract the HTML table into an R data frame.
Next, a set of heuristics is used to convert the R data frame to follow the ARD standard. Below is a record of these heuristics, which informs the parsing rules followed in `R/parse.R`:

1. Heuristic placeholder 1
2. Heuristic placeholder 2

## Coverage
As the RTF tables this package attempts to convert are designed to be human readable, and not machine readable, this package will never be able to guarantee 100% coverage across all tables.
Instead the heuristics described above should continually be updated inline with any changes to the code made to accomodate new RTF tables which did not fit the previous rule set.
