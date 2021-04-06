#' Get sponsor information for a bill
#'
#' @inheritParams getLegislation
#' @param billId Character vector containing the bill(s) to be retrieved.
#'      Each argument should take the form "XX YYYY", where XX
#'      is the prefix (HB, SB, etc.) and YYYY is the bill number.
#'
#' @return By default, returns a dataframe. If \code{as.xml = TRUE}, then
#'     returns the raw XML
#' @export
#'
#' @examples
#' ## get the list of all sponsors on a set of bills, filtered for primary sponsorship
#'
#' spons <- getBillSponsors("2007-08", c("HB 1001", "HB 1002", "HB 1003"))
#' spons <- subset(spons, Type == "Primary")
getBillSponsors <- function(biennium, billId, as.xml = FALSE) {
  if(!all(grepl(biennium_pattern, biennium))) {
    stop("Biennium formatted incorrectly. Use ?getBillSponsors for more information")
  } else if(!all(as.numeric(substr(biennium,1,4)) >= 1991)) {
    stop("Biennium out of range. Information is available going back to 1991-92")
  } else if(!all(grepl(billId_pattern, billId))) {
    stop("Bill ID formatted incorrectly. Use ?getBillSponsors for more information")
  }

  if(length(biennium) == length(billId)) {
    request <- data.frame(biennium = biennium, billId = billId)
  } else {
    request <- expand.grid(biennium, billId, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  }

  path <- paste(prefix,
                "legislationservice.asmx/GetSponsors?biennium=",
                request[1,1], "&billId=", request[1,2], sep = "")

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
                    "legislationservice.asmx/GetSponsors?biennium=",
                    request[bill,1], "&billId=", request[bill,2], sep = "")

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
