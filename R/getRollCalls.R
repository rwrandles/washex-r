#' Get roll call votes
#'
#' Get an XML containing roll call information for all recorded votes
#'     on a bill
#'
#' @inheritParams getLegislation
#'
#' @return Currently, returns an XML array containing all roll call votes.
#'     For usage, see examples below
#' @export
#'
#' @examples
#' votes <- getRollCalls("2007-08", "1001") # get roll call votes
#'
#' length(votes) # total number of roll call votes recorded
#'
#' names(votes[[1]]) # view all possible subcategories under roll call vote #1
#'
#' votes[[1]][["Motion"]][[1]] # motion title on roll call vote #1
#'
#' votes[[3]][["YeaVotes"]][["Count"]] # number of yea votes on roll call vote #3
#'
#' @section Note: This function as implemented is not in its final state.
#'     The resulting XML document(s) is heavily nested and
#'     typically requires further cleaning.
#'     See examples below for some potential usages. Further
#'     revisions will be made to help facilitate the XML parsing and
#'     potentially allow for compatibility with dataframes.
getRollCalls <- function(biennium, billNumber) {
  if(!all(grepl(biennium_pattern, biennium))) {
    stop("Biennium formatted incorrectly. Use ?getRollCalls for more information")
  } else if(!all(as.numeric(substr(biennium,1,4)) >= 1991)) {
    stop("Biennium out of range. Information is available going back to 1991-92")
  } else if(!all(grepl(billNum_pattern, billNumber))) {
    stop("Bill Number formatted incorrectly. Use ?getRollCalls for more information")
  }

  path <- paste(prefix,
                "legislationservice.asmx/GetRollCalls?biennium=",
                biennium, "&billNumber=", billNumber, sep = "")

  out <- tryCatch(XML::xmlTreeParse(path)[["doc"]][["ArrayOfRollCall"]],
           error = function(e){
             e$message <- errMessage
             stop(e)
           })

  return(out)
}
