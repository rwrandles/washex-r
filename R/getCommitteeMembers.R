#' Get committee members
#'
#' @inheritParams getLegislation
#' @param agency either "House" or "Senate", or a vector of agencies
#' @param name name of the committee or vector of committee names
#'     (for a list of committee names, see \code{\link{getCommittees}})
#'
#' @return By default, returns a dataframe. If \code{as.xml = TRUE}, then
#'     returns the raw XML
#' @export
#'
#' @examples
#' ## get all committee members for a select number of committees and years
#' years <- c("2011-12","2013-14","2015-16","2017-18")
#' comms <- c("Education","Judiciary","Rules")
#'
#' getCommitteeMembers(years,agency = "House",comms)
getCommitteeMembers <- function(biennium, agency, name, as.xml = FALSE) {
  if(length(biennium) == length(agency) &
     length(biennium) == length(name)) {
    request <- data.frame(biennium = biennium, agency = agency, name = name)
  } else {
    request <- expand.grid(biennium, agency, name, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  }

  path <- paste(prefix,
                "CommitteeService.asmx/GetCommitteeMembers?biennium=",
                request[1,1], "&agency=", request[1,2], "&committeeName=",
                gsub("&", "%26", gsub(" ", "%20", request[1,3])), sep = "")

  tbl <- XML::xmlParse(path)

  if(as.xml) {
    out <- tbl
  } else {
    out <- tibble::as_tibble(XML::xmlToDataFrame(tbl,
                                                 stringsAsFactors = FALSE))
    out$Biennium <- request[1,1]
    out$Agency <- request[1,2]
    out$Name <- request[1,3]
  }

  if(nrow(request) > 1) {
    for(bill in 2:nrow(request)) {
      path <- paste(prefix,
                    "CommitteeService.asmx/GetCommitteeMembers?biennium=",
                    request[bill,1], "&agency=", request[bill,2], "&committeeName=",
                    gsub("&", "%26", gsub(" ", "%20", request[bill,3])), sep = "")

      tbl <- XML::xmlParse(path)

      if(as.xml) {
        out <- c(out,tbl)
      } else {
        tbl <- tibble::as_tibble(XML::xmlToDataFrame(tbl,
                                                     stringsAsFactors = FALSE))
        tbl$Biennium <- request[bill,1]
        tbl$Agency <- request[bill,2]
        tbl$Name <- request[bill,3]

        out <- rbind(out, tbl)
      }
    }
  }

  if(as.xml & nrow(request) > 1) {
    names(out) <- paste(request[,1],request[,2],request[,3],sep="//")
  }
  if(!as.xml) {
    out <- out[c("Biennium","Agency","Name",
                 setdiff(names(out),c("Biennium", "Agency", "Name")))]
  }
  return(out)
}
