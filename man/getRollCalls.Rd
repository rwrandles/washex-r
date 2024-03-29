% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getRollCalls.R
\name{getRollCalls}
\alias{getRollCalls}
\alias{getRollCalls.xml}
\alias{getRollCalls.summary}
\alias{getRollCalls.votes}
\title{Get roll call votes}
\usage{
getRollCalls.xml(biennium, billNumber, paired = TRUE)

getRollCalls.summary(
  biennium,
  billNumber,
  paired = TRUE,
  type = c("df", "list")
)

getRollCalls.votes(biennium, billNumber, paired = TRUE, type = c("df", "list"))
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
\code{getRollCalls.xml} returns a list of XML objects for each bill.
\code{getRollCalls.summary} and \code{getRollCalls.votes}
return objects of type equal to the
\code{type} argument (defaults to dataframe)
}
\description{
Get an XML containing roll call information for all recorded votes
on a bill
}
\section{Note}{
 Due to the nested nature of the resulting document,
we provide various functions to present simplified views of the data
that are compatible with more parsimonious data structures. To see the
full, original data, use \code{getRollCalls.xml} instead.
}

\examples{
votes <- getRollCalls.summary("2007-08", "1001") # get roll call votes
if(!is.null(votes)) {
  length(votes) # total number of roll call votes recorded
  votes$CountYeas[3] # number of yea votes on roll call vote #3
}

## example: get member id's for all representatives voting against the bill
## on final passage
votes <- getRollCalls.votes("2007-08", "1001")
if(!is.null(votes)) {
  nay_votesFP <- subset(votes, (Motion == "Final Passage" & Vote == "Nay"))
  print(nay_votesFP$MemberId)
}

}
