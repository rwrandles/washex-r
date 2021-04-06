#' Search for bills based on Revised Code (RCW) citations
#'
#' Get a list of all bills which reference or amend a particular
#'      portion of the Revised Code of Washington (RCW)
#'
#' @inheritParams getLegislation
#' @param rcwCite Character vector for the citation in the RCW to pull
#'      legislation from. Optional extensions for title, chapter, and section
#'      are allowed. For more information, see
#'      \url{https://apps.leg.wa.gov/rcw/}
#'
#' @return default, returns a dataframe. If \code{as.xml = TRUE}, then
#'     returns the raw XML
#' @export
#'
#' @examples
#' getRCWBills("2007-08", "13.40.0357")
getRCWBills <- function(biennium, rcwCite, paired = FALSE, as.xml = FALSE) {
  if(!all(grepl(biennium_pattern, biennium))) {
    stop("Biennium formatted incorrectly. Use ?getLegislation for more information")
  } else if(!all(as.numeric(substr(biennium,1,4)) >= 1991)) {
    stop("Biennium out of range. Information is available going back to 1991-92")
  } else if(!all(grepl(rcw_pattern, rcwCite))) {
    stop("RCW reference formatted incorrectly. Use ?getRCWBills for more information")
  }

  if(length(biennium) == length(rcwCite) & paired) {
    request <- data.frame(biennium = biennium, rcwCite = rcwCite)
  } else {
    request <- expand.grid(biennium, rcwCite, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  }

  path <- paste(prefix,
                "RcwCiteAffectedService.asmx/GetLegislationAffectingRcw?biennium=",
                request[1,1], "&rcwCite=", request[1,2], sep = "")

  tbl <- tryCatch(XML::xmlParse(path),
                  error = function(e){
                    e$message <- errMessage
                    stop(e)
                  })

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

      tbl <- tryCatch(XML::xmlParse(path),
                      error = function(e){
                        e$message <- errMessage
                        stop(e)
                      })

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
