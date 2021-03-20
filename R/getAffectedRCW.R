#' Link bills to Revised Code of Washington (RCW)
#'
#' Get a listing of all RCW citations affected by a given bill
#'
#' @inheritParams getBillSponsors
#'
#' @return By default, returns a dataframe. If \code{as.xml = TRUE}, then
#'     returns the raw XML
#'
#' @export
#' @examples
#' ## usage for a single bill case, XML form
#' getAffectedRCW("2005-06", "HB 1427", as.xml = TRUE)
#'
#' ## generates a dataframe of affected codes from all bills in 2007-08
#' bills <- getLegislationByYear(c("2007","2008"))
#'
#' \dontrun{codesAffected <- getAffectedRCW("2007-08", bills$BillId)}
getAffectedRCW <- function(biennium, billId, as.xml = FALSE) {
  if(!all(grepl(biennium_pattern, biennium))) {
    stop("Biennium formatted incorrectly. Use ?getAffectedRCW for more information")
  } else if(!all(as.numeric(substr(biennium,1,4)) >= 1991)) {
    stop("Biennium out of range. Information is available going back to 1991-92")
  } else if(!all(grepl(billId_pattern, billId))) {
    stop("Bill ID formatted incorrectly. Use ?getAffectedRCW for more information")
  }

  if(length(biennium) == length(billId)) {
    request <- data.frame(biennium = biennium, billId = billId)
  } else {
    request <- expand.grid(biennium, billId, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  }

  path <- paste(prefix,
                "legislationservice.asmx/GetRcwCitesAffected?biennium=",
                request[1,1], "&billId=", gsub(" ", "%20", request[1,2]), sep = "")

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
    out$BillId <- request[1,2]
  }

  if(nrow(request) > 1) {
    for(bill in 2:nrow(request)) {
      path <- paste(prefix,
                    "legislationservice.asmx/GetRcwCitesAffected?biennium=",
                    request[bill,1], "&billId=", gsub(" ", "%20", request[bill,2]), sep = "")

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
        tbl$BillId <- request[bill,2]

        out <- rbind(out, tbl)
      }
    }
  }

  if(as.xml & nrow(request) > 1) {
    names(out) <- paste(request[,1],request[,2],sep="//")
  }
  if(!as.xml) {
    out <- out[c("Biennium","BillId",
                 setdiff(names(out),c("Biennium","BillId")))]
  }
  return(out)
}
