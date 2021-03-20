#' Get summary information on a particular bill
#'
#' Get legislative summary information for a particular bill,
#'     including bill ID, introduction date, bill title(s), and
#'     description
#'
#' @param biennium either a string or character vector of the form "XXXX-YY"
#' @param billNumber an integer or character string or vector containing
#'      the bill numbers to retrieve
#' @param as.xml set to TRUE for raw XML (defaults to FALSE)
#'
#' @return By default, returns a dataframe. If \code{as.xml = TRUE}, then
#'     returns the raw XML
#' @export
#'
#' @examples
#' getLegislation("2007-08", "1001")
#' getLegislation("2007-08", 1001, as.xml = TRUE)
getLegislation <- function(biennium, billNumber, as.xml = FALSE) {
  if(!all(grepl(biennium_pattern, biennium))) {
    stop("Biennium formatted incorrectly. Use ?getLegislation for more information")
  } else if(!all(as.numeric(substr(biennium,1,4)) >= 1991)) {
    stop("Biennium out of range. Information is available going back to 1991-92")
  } else if(!all(grepl(billNum_pattern, billNumber))) {
    stop("Bill Number formatted incorrectly. Use ?getLegislation for more information")
  }

  if(length(biennium) == length(billNumber)) {
    request <- data.frame(biennium = biennium, billId = billNumber)
  } else {
    request <- expand.grid(biennium, billNumber, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  }

  path <- paste(prefix, "legislationservice.asmx/GetLegislation?biennium=",request[1,1],
                "&billNumber=", gsub(" ", "%20", request[1,2]), sep = "")

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
      path <- paste(prefix, "legislationservice.asmx/GetLegislation?biennium=",request[bill,1],
                    "&billNumber=", gsub(" ", "%20", request[bill,2]), sep = "")

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
