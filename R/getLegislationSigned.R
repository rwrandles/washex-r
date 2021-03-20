#' Get all bills signed into law
#'
#' Get a dataframe containing all of the bills that originated in a
#'     given chamber and were eventually signed into law
#'
#' @inheritParams getCommitteeMembers
#'
#' @return By default, returns a dataframe. If \code{as.xml = TRUE}, then
#'     returns the raw XML
#' @export
#'
#' @examples
#' getLegislationSigned("2007-08", "Senate", as.xml = TRUE)
getLegislationSigned <- function(biennium, agency, as.xml = FALSE) {
  if(!all(grepl(biennium_pattern, biennium))) {
    stop("Biennium formatted incorrectly. Use ?getLegislationSigned for more information")
  } else if(!all(as.numeric(substr(biennium,1,4)) >= 1991)) {
    stop("Biennium out of range. Information is available going back to 1991-92")
  }

  agency <- paste(toupper(substr(agency,1,1)),
                  substr(agency,2,nchar(agency)), sep = "")

  if(!all(agency %in% c("House", "Senate"))) {
    stop("Agency name invalid. Make sure to use one of 'House' or 'Senate'")
  }

  if(length(biennium) == length(agency)) {
    request <- data.frame(biennium = biennium, agency = agency)
  } else {
    request <- expand.grid(biennium, agency, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  }

  path <- paste(prefix,
                "legislationservice.asmx/GetLegislationGovernorSigned?biennium=",
                request[1,1], "&agency=", request[1,2], sep = "")

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
    out$Agency <- request[1,2]
  }

  if(nrow(request) > 1) {
    for(bill in 2:nrow(request)) {
      path <- paste(prefix,
                    "legislationservice.asmx/GetLegislationGovernorSigned?biennium=",
                    request[bill,1], "&agency=", request[bill,2], sep = "")

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
        tbl$Agency <- request[bill,2]

        out <- rbind(out, tbl)
      }
    }
  }

  if(as.xml & nrow(request) > 1) {
    names(out) <- paste(request[,1],request[,2],sep="//")
  }
  if(!as.xml) {
    out <- out[c("Biennium","Agency",
                 setdiff(names(out),c("Biennium", "Agency")))]
  }
  return(out)
}
