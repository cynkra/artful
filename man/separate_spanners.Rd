% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parse.R
\name{separate_spanners}
\alias{separate_spanners}
\title{Separate a spanner}
\usage{
separate_spanners(data)
}
\arguments{
\item{data}{A data frame generated by calling \code{\link[=rtf_to_html]{rtf_to_html()}}.}
}
\value{
A data frame where merged columns have been separated.
}
\description{
This function separates spanning column labels out inside a data frame
generated by calling \code{\link[=rtf_to_html]{rtf_to_html()}}. A spanning column header represents a
unique grouping identifier for the columns it spans over.
}
\keyword{internal}
