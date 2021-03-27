#' Get hearings regarding a bill
#'
#' Get a list of dates, locations, and descriptions, of all
#'     committee hearings on a particular bill
#'
#' @inheritParams getLegislation
#'
#' @return By default, returns a list of hearings. If \code{as.xml = TRUE}, then
#'     returns the raw XML
#' @export
#'
#' @examples
#' getHearings("2007-08", "1001", as.xml = FALSE)
#'
#' @section Note: The resulting XML documents contain multiple
#'      nested lists. As such, it is not currently compatible with the XML
#'      package's conversion to the dataframe format. Future revisions
#'      may be made in order to help with the cleaning process and potentially
#'      allow for better compatibility with dataframes.
getHearings <- function(biennium, billNumber, paired = TRUE, as.xml = FALSE) {
  if(!all(grepl(biennium_pattern, biennium))) {
    stop("Biennium formatted incorrectly. Use ?getHearings for more information")
  } else if(!all(as.numeric(substr(biennium,1,4)) >= 1991)) {
    stop("Biennium out of range. Information is available going back to 1991-92")
  } else if(!all(grepl(billNum_pattern, billNumber))) {
    stop("Bill Number formatted incorrectly. Use ?getHearings for more information")
  }

  if(length(biennium) == length(billNumber) & paired) {
    request <- data.frame(biennium = biennium, billNumber = billNumber)
  } else {
    request <- expand.grid(biennium, billNumber, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  }
  path <- paste(prefix,
                "legislationservice.asmx/GetHearings?biennium=",
                request[1,1], "&billNumber=", request[1,2], sep = "")

  tbl <- tryCatch(XML::xmlParse(path),
                  error = function(e){
                    e$message <- errMessage
                    stop(e)
                  })

  if(as.xml) {
    out <- tbl
  } else {
    out <- list("Bill"=XML::xmlToList(tbl))
  }

  if(nrow(request) > 1) {
    for(bill in 2:nrow(request)) {
      path <- paste(prefix,
                    "legislationservice.asmx/GetHearings?biennium=",
                    request[bill,1], "&billNumber=", request[bill,2], sep = "")

      tbl <- tryCatch(XML::xmlParse(path),
                      error = function(e){
                        e$message <- errMessage
                        stop(e)
                      })

      if(!as.xml) {
        tbl <- list("Bill"=XML::xmlToList(tbl))
      }

      out <- c(out,tbl)
    }
  }

  if(as.xml & nrow(request) > 1) {
    names(out) <- paste(request[,1],request[,2],sep="//")
  }
  if(!as.xml) {
    names(out) <- paste(request[,1],request[,2],sep="//")
  }
  return(out)
}
