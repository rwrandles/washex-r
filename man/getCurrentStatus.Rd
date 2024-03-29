% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getCurrentStatus.R
\name{getCurrentStatus}
\alias{getCurrentStatus}
\title{Get bill status}
\usage{
getCurrentStatus(
  biennium,
  billNumber,
  paired = TRUE,
  type = c("df", "list", "xml")
)
}
\arguments{
\item{biennium}{Character vector representing the biennium(s) to be
searched. Each argument should take the form "XXXX-YY"}

\item{billNumber}{Character or numeric vector containing the bill number(s)
to be retrieved.}

\item{paired}{If TRUE, will assume that equal length vectors represent
paired data. Set to FALSE to generate an NxN grid of input
arguments. Applies to equal length vector inputs only.}

\item{type}{One of "df", "list", or "xml". Specifies the format for
the output.}
}
\value{
\code{getCurrentStatus} returns an object of type equal to the
\code{type} argument (defaults to dataframe)
}
\description{
Get the current status of a given bill
}
\section{Note}{

This function returns the bill's status as of today. If a bill
was never passed, it lists the most recent status. To
get a bill's complete history, use \code{\link{getStatusChanges}}
}

\examples{
getCurrentStatus("2007-08", "1001")

## get final status for all bills written in 2011
\dontrun{
billNums <- getLegislationByYear("2011")
status <- getCurrentStatus("2011-12", billNums$billNumber)}

}
