#' Get amendments to a bill
#'
#' Get a list of all proposed amendments (accepted and rejected) on the bill,
#'     including the URL to the amendment text
#'
#' @inheritParams getLegislation
#'
#' @return By default, returns a dataframe. If \code{as.xml = TRUE}, then
#'     returns the raw XML
#' @export
#'
#' @examples
#' ## get amendments for a single bill
#' getAmendments("2007-08", "1001", as.xml = FALSE)
#'
#' ## get amendments for a specific set of bills
#' years <- c("2005-06","2007-08","2007-08","2009-10")
#' bills <- c(1447,1219,1001,2680)
#'
#' getAmendments(years, bills, paired = TRUE, as.xml = FALSE)
getAmendments <- function(biennium, billNumber, paired = TRUE, as.xml = FALSE) {
  if(!all(grepl(biennium_pattern, biennium))) {
    stop("Biennium formatted incorrectly. Use ?getAmendments for more information")
  } else if(!all(as.numeric(substr(biennium,1,4)) >= 1991)) {
    stop("Biennium out of range. Information is available going back to 1991-92")
  } else if(!all(grepl(billNum_pattern, billNumber))) {
    stop("Bill Number formatted incorrectly. Use ?getAmendments for more information")
  }

  if(length(biennium) == length(billNumber) & paired) {
    request <- data.frame(biennium = biennium, billNumber = billNumber)
  } else {
    request <- expand.grid(biennium, billNumber, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  }

  path <- paste(prefix,
                "legislationservice.asmx/GetAmendmentsForBiennium?biennium=",
                request[1,1], "&billNumber=", request[1,2], sep = "")

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
    out$BillNumber <- request[1,2]
    out <- out[c(setdiff(names(out),"Agency"))]
  }

  if(nrow(request) > 1) {
    for(bill in 2:nrow(request)) {
      path <- paste(prefix,
                    "legislationservice.asmx/GetAmendmentsForBiennium?biennium=",
                    request[bill,1], "&billNumber=", request[bill,2], sep = "")

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
        tbl$BillNumber <- request[bill,2]

        tbl <- tbl[c(setdiff(names(tbl),"Agency"))]

        out <- rbind(out, tbl)
      }
    }
  }

  if(as.xml & nrow(request) > 1) {
    names(out) <- paste(request[,1],request[,2],sep="//")
  }
  if(!as.xml) {
    out <- out[c("Biennium","BillNumber",
                 setdiff(names(out),c("Biennium","BillNumber")))]
  }
  return(out)
}
