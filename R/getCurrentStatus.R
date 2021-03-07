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
#' @section Note:
#' This function returns the bill's status as of today. If a bill
#'     was never passed, it lists the most recent status. To
#'     get a bill's complete history, use \code{\link{getStatusChanges}}
getCurrentStatus <- function(biennium, billNumber, as.xml = FALSE) {
  if(length(biennium) == length(billNumber)) {
    request <- data.frame(biennium = biennium, billNumber = billNumber)
  } else {
    request <- expand.grid(biennium, billNumber, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  }

  path <- paste(prefix, "legislationservice.asmx/GetCurrentStatus?biennium=",
                gsub(" ", "%20", request[1,1]), "&billNumber=",
                gsub(" ", "%20", request[1,2]), sep = "")

  tbl <- XML::xmlParse(path)

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

      tbl <- XML::xmlParse(path)

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
