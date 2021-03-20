#' Get legislators
#'
#' Get a list of all sponsors (all congressmembers) for a given biennium
#'
#' @inheritParams getLegislation
#'
#' @return By default, returns a dataframe. If \code{as.xml = TRUE}, then
#'     returns the raw XML
#' @export
#'
#' @examples
#' getSponsors("2007-08")
getSponsors <- function(biennium, as.xml = FALSE) {
  if(!all(grepl(biennium_pattern, biennium))) {
    stop("Biennium formatted incorrectly. Use ?getSponsors for more information")
  } else if(!all(as.numeric(substr(biennium,1,4)) >= 1991)) {
    stop("Biennium out of range. Information is available going back to 1991-92")
  }

  path <- paste(prefix,
                "sponsorservice.asmx/GetSponsors?biennium=",
                biennium[1], sep = "")

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
    out$Biennium <- biennium[1]
  }

  if(length(biennium) > 1) {
    for(i in 2:length(biennium)) {
      path <- paste(prefix,
                    "sponsorservice.asmx/GetSponsors?biennium=",
                    biennium[i], sep = "")

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
        tbl$Biennium <- biennium[i]

        out <- rbind(out, tbl)
      }
    }
  }

  if(as.xml & length(biennium) > 1) {
    names(out) <- biennium
  }
  if(!as.xml) {
    out <- out[c("Biennium",
                 setdiff(names(out),c("Biennium")))]
  }
  return(out)
}
