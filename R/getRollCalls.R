#' Get roll call votes
#'
#' Get an XML containing roll call information for all recorded votes
#'     on a bill
#'
#' @inheritParams getLegislation
#'
#' @return Currently, returns an XML array containing all roll call votes.
#'     For usage, see examples below
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
#'
#' @name getRollCalls
NULL

#' @export
#' @rdname getRollCalls
getRollCalls.xml <- function(biennium, billNumber, paired = TRUE) {
  if(!all(grepl(biennium_pattern, biennium))) {
    stop("Biennium formatted incorrectly. Use ?getRollCalls for more information")
  } else if(!all(as.numeric(substr(biennium,1,4)) >= 1991)) {
    stop("Biennium out of range. Information is available going back to 1991-92")
  } else if(!all(grepl(billNum_pattern, billNumber))) {
    stop("Bill Number formatted incorrectly. Use ?getRollCalls for more information")
  }

  if(length(biennium) == length(billNumber) & paired) {
    request <- data.frame(biennium = biennium, billNumber = billNumber)
  } else {
    request <- expand.grid(biennium, billNumber, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  }

  out <- c()

  for(bill in 1:nrow(request)) {
    path <- paste(prefix,
                  "legislationservice.asmx/GetRollCalls?biennium=",
                  biennium, "&billNumber=", billNumber, sep = "")

    tbl <- tryCatch(XML::xmlParse(path),
                    error = function(e) {
                      e$message <- errMessage
                      stop(e)
                    })

    out <- c(out, tbl)
  }
  names(out) <- paste(request[,1], request[,2], sep = "//")
  return(out)
}

#' @export
#' @rdname getRollCalls
getRollCalls.summary <- function(biennium, billNumber, paired = TRUE, type = c("df", "list")) {
  type <- rlang::arg_match(type)

  if(!all(grepl(biennium_pattern, biennium))) {
    stop("Biennium formatted incorrectly. Use ?getRollCalls for more information")
  } else if(!all(as.numeric(substr(biennium,1,4)) >= 1991)) {
    stop("Biennium out of range. Information is available going back to 1991-92")
  } else if(!all(grepl(billNum_pattern, billNumber))) {
    stop("Bill Number formatted incorrectly. Use ?getRollCalls for more information")
  }

  if(length(biennium) == length(billNumber) & paired) {
    request <- data.frame(biennium = biennium, billNumber = billNumber)
  } else {
    request <- expand.grid(biennium, billNumber, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  }

  if(type == "df") {
    out <- data.frame()

    for(bill in 1:nrow(request)) {
      xml <- unname(getRollCalls.xml(request[bill,1], request[bill,2]))

      tbl <- XML::xmlToList(xml[[1]])

      if(length(tbl) > 0) {
        tbl <- purrr::map(tbl, ~ data.frame(Agency = .x[["Agency"]],
                                          BillId = .x[["BillId"]],
                                          Biennium = .x[["Biennium"]],
                                          Motion = .x[["Motion"]],
                                          SequenceNumber = .x[["SequenceNumber"]],
                                          VoteDate = .x[["VoteDate"]],
                                          CountYeas = .x[["YeaVotes"]]$Count,
                                          CountNays = .x[["NayVotes"]]$Count,
                                          CountAbsent = .x[["AbsentVotes"]]$Count,
                                          CountExcused = .x[["ExcusedVotes"]]$Count))

        df <- dplyr::bind_rows(tbl)
        out <- dplyr::bind_rows(out, df)
      }
    }
  } else if(type == "list") {
    out <- list()

    for(bill in 1:nrow(request)) {
      xml <- unname(getRollCalls.xml(request[bill,1], request[bill,2]))

      tbl <- purrr::map(XML::xmlToList(xml[[1]]), purrr::flatten)
      tbl <- purrr::map(tbl, ~ toss(.x, c("MembersVoting", "Vote")))
      #tbl <- purrr::map(tbl, ~ toss(.x, "Vote"))
      if(length(tbl) > 0) {
        for(i in 1:length(tbl)) {
          names(tbl[[i]]) <- c("Agency",
                             "BillId",
                             "Biennium",
                             "Motion",
                             "SequenceNumber",
                             "VoteDate",
                             "CountYeas",
                             "CountNays",
                             "CountAbsent",
                             "CountExcused")
        }
        list <- list(tbl)
        names(list) <- request[bill,2]
        out <- c(out, list)
      }
    }
  }
  return(out)
}

#' @export
#' @rdname getRollCalls
getRollCalls.votes <- function(biennium, billNumber, paired = TRUE, type = c("df", "list")) {
  type <- rlang::arg_match(type)

  if(!all(grepl(biennium_pattern, biennium))) {
    stop("Biennium formatted incorrectly. Use ?getRollCalls for more information")
  } else if(!all(as.numeric(substr(biennium,1,4)) >= 1991)) {
    stop("Biennium out of range. Information is available going back to 1991-92")
  } else if(!all(grepl(billNum_pattern, billNumber))) {
    stop("Bill Number formatted incorrectly. Use ?getRollCalls for more information")
  }

  if(length(biennium) == length(billNumber) & paired) {
    request <- data.frame(biennium = biennium, billNumber = billNumber)
  } else {
    request <- expand.grid(biennium, billNumber, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  }

  if(type == "df") {
    out <- data.frame()

    for(bill in 1:nrow(request)) {
      xml <- unname(getRollCalls.xml(request[bill,1], request[bill,2]))

      tbl <- XML::xmlToList(xml[[1]])
      tbl <- purrr::map(tbl, purrr::flatten)
      tbl <- purrr::map(tbl, ~ toss(.x, c("SequenceNumber", "Count", "MembersVoting")))

      if(length(tbl) > 0) {
        tbl <- purrr::map(tbl, ~ data.frame(Agency = .x[["Agency"]],
                                            BillId = .x[["BillId"]],
                                            Biennium = .x[["Biennium"]],
                                            Motion = .x[["Motion"]],
                                            VoteDate = .x[["VoteDate"]],
                                            MemberId = unlist(purrr::map(.x, "MemberId")),
                                            Name = unlist(purrr::map(.x, "Name")),
                                            Vote = unlist(purrr::map(.x, "VOte"))))
        # yes there is supposed to be a typo on "VOte"

        df <- dplyr::bind_rows(tbl)
        out <- dplyr::bind_rows(out, df)
      }
    }
  } else if(type == "list") {
    out <- list()

    for(bill in 1:nrow(request)) {
      xml <- unname(getRollCalls.xml(request[bill,1], request[bill,2]))

      tbl <- XML::xmlToList(xml[[1]])
      tbl <- purrr::map(tbl, purrr::flatten)
      tbl <- purrr::map(tbl, ~ toss(.x, c("SequenceNumber", "Count", "MembersVoting")))

      if(length(tbl) > 0) {
        list <- list(tbl)
        names(list) <- request[bill,2]
        out <- c(out, list)
      }
    }
  }
  return(out)
}
