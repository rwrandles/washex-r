#' Get legislation by year
#'
#' Get a list of all bills introduced during the year
#'
#' @inheritParams getLegislation
#' @param year an integer or character string or vector for the given year
#'
#' @return By default, returns a dataframe. If \code{as.xml = TRUE}, then
#'     returns the raw XML
#' @export
#'
#' @examples
#' getLegislationByYear("2007")
getLegislationByYear <- function(year,as.xml = FALSE) {
  path <- paste(prefix, "legislationservice.asmx/GetLegislationByYear?year=",
                gsub(" ", "%20", year[1]), sep = "")

  tbl <- XML::xmlParse(path)

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

      tbl <- XML::xmlParse(path)

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
