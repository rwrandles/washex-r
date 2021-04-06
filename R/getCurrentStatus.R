#' Get bill status
#'
#' Get the current status of a given bill
#'
#' @inheritParams getLegislation
#'
#' @return By default, returns a dataframe. If \code{as.xml = TRUE}, then
#'     returns the raw XML
#' @export
#'
#' @examples
#' getCurrentStatus("2007-08", "1001")
#'
#' ## get final status for all bills written in 2011 or 2012
#' billNums <- rbind(getLegislationByYear("2011"), getLegislationByYear("2012"))
#' \dontrun{status <- getCurrentStatus("2011-12", billNums$billNumber)}
#'
#' @section Note:
#' This function returns the bill's status as of today. If a bill
#'     was never passed, it lists the most recent status. To
#'     get a bill's complete history, use \code{\link{getStatusChanges}}
getCurrentStatus <- function(biennium, billNumber, paired = TRUE, as.xml = FALSE) {
  if(!all(grepl(biennium_pattern, biennium))) {
    stop("Biennium formatted incorrectly. Use ?getCurrentStatus for more information")
  } else if(!all(as.numeric(substr(biennium,1,4)) >= 1991)) {
    stop("Biennium out of range. Information is available going back to 1991-92")
  } else if(!all(grepl(billNum_pattern, billNumber))) {
    stop("Bill Number formatted incorrectly. Use ?getCurrentStatus for more information")
  }

  if(length(biennium) == length(billNumber) & paired) {
    request <- data.frame(biennium = biennium, billNumber = billNumber)
  } else {
    request <- expand.grid(biennium, billNumber, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  }

  path <- paste(prefix, "legislationservice.asmx/GetCurrentStatus?biennium=",
                gsub(" ", "%20", request[1,1]), "&billNumber=",
                gsub(" ", "%20", request[1,2]), sep = "")

  tbl <- tryCatch(XML::xmlParse(path),
                  error = function(e){
                    e$message <- errMessage
                    stop(e)
                  })

  if(as.xml) {
    out <- tbl
  } else {
    tbl <- XML::xmlToList(tbl)
    out <- data.frame(t(matrix(unlist(tbl),dimnames=list(names(tbl)))))

    out$Biennium <- request[1,1]
    out$BillNumber <- request[1,2]
  }

  if(nrow(request) > 1) {
    for(bill in 2:nrow(request)) {
      path <- paste(prefix, "legislationservice.asmx/GetCurrentStatus?biennium=",
                    gsub(" ", "%20", request[bill,1]), "&billNumber=",
                    gsub(" ", "%20", request[bill,2]), sep = "")

      tbl <- tryCatch(XML::xmlParse(path),
                      error = function(e){
                        e$message <- errMessage
                        stop(e)
                      })

      if(as.xml) {
        out <- c(out,tbl)
      } else {
        tbl <- XML::xmlToList(tbl)
        tbl <- data.frame(t(matrix(unlist(tbl),dimnames=list(names(tbl)))))

        tbl$Biennium <- request[bill,1]
        tbl$BillNumber <- request[bill,2]

        out <- rbind(out,tbl)
      }
    }
  }

  if(as.xml & nrow(request) > 1) {
    names(out) <- paste(request[,1],request[,2],sep = "//")
  }
  if(!as.xml) {
    out <- out[c("Biennium","BillNumber",
                 setdiff(names(out),c("Biennium","BillNumber")))]
  }
  return(out)
}
