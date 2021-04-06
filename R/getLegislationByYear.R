#' Get legislation by year
#'
#' Get a list of all bills introduced during the year
#'
#' @inheritParams getLegislation
#' @param year Character or numeric vector representing the year(s) to be
#'      searched.
#'
#' @return By default, returns a dataframe. If \code{as.xml = TRUE}, then
#'     returns the raw XML
#' @export
#'
#' @examples
#' getLegislationByYear("2007")
getLegislationByYear <- function(year,as.xml = FALSE) {
  if(!all(grepl(year_pattern, year))) {
    stop("Year formatted incorrectly. Use ?getLegislationByYear for more information")
  } else if(!all(as.numeric(year) >= 1991)) {
    stop("Year out of range. Information is available going back to 1991")
  }

  path <- paste(prefix, "legislationservice.asmx/GetLegislationByYear?year=",
                gsub(" ", "%20", year[1]), sep = "")

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
  }

  if(length(year) > 1) {
    for(i in 2:length(year)) {
      path <- paste(prefix, "legislationservice.asmx/GetLegislationByYear?year=",
                    gsub(" ", "%20", year[i]), sep = "")

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

        out <- rbind(out, tbl)
      }
    }
  }

  if(as.xml & length(year) > 1) {
    names(out) <- year
  }
  return(out)
}
