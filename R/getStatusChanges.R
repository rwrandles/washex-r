#' Track historical progess on a bill
#'
#' Get a complete history of all status changes that occurred on a particular
#'     bill
#'
#' @inheritParams getLegislation
#'
#' @return By default, returns a dataframe. If \code{as.xml = TRUE}, then
#'     returns the raw XML
#' @export
#'
#' @examples
#' getStatusChanges("2007-08", "1001", as.xml = FALSE)
getStatusChanges <- function(biennium, billNumber, paired = TRUE, as.xml = FALSE) {
  if(!all(grepl(biennium_pattern, biennium))) {
    stop("Biennium formatted incorrectly. Use ?getStatusChanges for more information")
  } else if(!all(as.numeric(substr(biennium,1,4)) >= 1991)) {
    stop("Biennium out of range. Information is available going back to 1991-92")
  } else if(!all(grepl(billNum_pattern, billNumber))) {
    stop("Bill Number formatted incorrectly. Use ?getStatusChanges for more information")
  }

  if(length(biennium) == length & paired) {
    request <- data.frame(biennium = biennium, billId = billNumber)
  } else {
    request <- expand.grid(biennium, billNumber, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  }

  beginDate <- paste(substr(request[1,1],1,4),"01","01",sep = "-")
  endDate <- paste(substr(request[1,1],1,2),substr(request[1,1],6,7),"-12","-31",sep = "")

  path <- paste(prefix,
                "legislationservice.asmx/GetLegislativeStatusChangesByBillNumber?biennium=",
                gsub(" ", "%20", request[1,1]), "&billNumber=", gsub(" ", "%20", request[1,2]),
                "&beginDate=", beginDate, "&endDate=", endDate, sep="")

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
  }

  if(nrow(request) > 1) {
    for(bill in 2:nrow(request)) {
      beginDate <- paste(substr(request[bill,1],1,4),"01","01",sep = "-")
      endDate <- paste(substr(request[bill,1],1,2),substr(request[bill,1],6,7),"-12","-31",sep = "")

      path <- paste(prefix,
                    "legislationservice.asmx/GetLegislativeStatusChangesByBillNumber?biennium=",
                    gsub(" ", "%20", request[bill,1]), "&billNumber=", gsub(" ", "%20", request[bill,2]),
                    "&beginDate=", beginDate, "&endDate=", endDate, sep="")

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
