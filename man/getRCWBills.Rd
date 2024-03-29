% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getRCWBills.R
\name{getRCWBills}
\alias{getRCWBills}
\title{Search for bills based on Revised Code (RCW) citations}
\usage{
getRCWBills(biennium, rcwCite, paired = FALSE, type = c("df", "list", "xml"))
}
\arguments{
\item{biennium}{Character vector representing the biennium(s) to be
searched. Each argument should take the form "XXXX-YY"}

\item{rcwCite}{Character vector for the citation in the RCW to pull
legislation from. Optional extensions for title, chapter, and section
are allowed. For more information, see
\url{https://apps.leg.wa.gov/rcw/}}

\item{paired}{If TRUE, will assume that equal length vectors represent
paired data. Set to FALSE to generate an NxN grid of input
arguments. Applies to equal length vector inputs only.}

\item{type}{One of "df", "list", or "xml". Specifies the format for
the output.}
}
\value{
\code{getRCWBills} returns an object of type equal to the
\code{type} argument (defaults to dataframe)
}
\description{
Get a list of all bills which reference or amend a particular
portion of the Revised Code of Washington (RCW)
}
\examples{
getRCWBills("2007-08", "13.40.0357")
}
