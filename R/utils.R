## negative of purr::keep("name")

toss <- function(.x, .p) {
  pat <- paste(paste0("^", .p, "$"), collapse = "|")
  return(.x[!grepl(pat, names(.x))])
}
