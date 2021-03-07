#' Search for bills based on Revised Code (RCW) citations
#'
#' Get a list of all bills which reference or amend the Revised Code of
#'     Washington (RCW)
#'
#' @inheritParams getLegislation
#' @param rcwCite character string (or vector) for the citation (optional extensions
#'     for title, chapter, or section) in the RCW to pull legislation
#'     from
#'
#' @return default, returns a dataframe. If \code{as.xml = TRUE}, then
#'     returns the raw XML
#' @export
#'
#' @examples
#' getRCWBills("2007-08", "13.40.0357")
getRCWBills <- function(biennium, rcwCite, as.xml = FALSE) {
  if(length(biennium) == length(rcwCite)) {
    request <- data.frame(biennium = biennium, rcwCite = rcwCite)
  } else {
    request <- expand.grid(biennium, rcwCite, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  }

  path <- paste(prefix,
                "RcwCiteAffectedService.asmx/GetLegislationAffectingRcw?biennium=",
                request[1,1], "&rcwCite=", request[1,2], sep = "")

  tbl <- XML::xmlParse(path)

  if(as.xml) {
    out <- tbl
  } else {
    out <- tibble::as_tibble(XML::xmlToDataFrame(tbl,
                                                 stringsAsFactors = FALSE))
    out$Biennium <- request[1,1]
    out$RcwCite <- request[1,2]
  }

  if(nrow(request) > 1) {
    for(bill in 2:nrow(request)) {
      path <- paste(prefix,
                    "RcwCiteAffectedService.asmx/GetLegislationAffectingRcw?biennium=",
                    request[bill,1], "&rcwCite=", request[bill,2], sep = "")

      tbl <- XML::xmlParse(path)

      if(as.xml) {
        out <- c(out,tbl)
      } else {
        tbl <- tibble::as_tibble(XML::xmlToDataFrame(tbl,
                                                     stringsAsFactors = FALSE))
        tbl$Biennium <- request[bill,1]
        tbl$RcwCite <- request[bill,2]

        out <- rbind(out, tbl)
      }
    }
  }

  if(as.xml & nrow(request) > 1) {
    names(out) <- paste(request[,1],request[,2],sep="//")
  }
  if(!as.xml) {
    out <- out[c("Biennium","RcwCite",
                 setdiff(names(out),c("Biennium","RcwCite")))]
  }
  return(out)
}
