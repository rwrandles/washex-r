test_that("function works for each input type", {
  d <- getCommitteeMembers("2007-08", "House", "Rules", type = "df")
  l <- getCommitteeMembers("2007-08", "House", "Rules", type = "list")
  x <- getCommitteeMembers("2007-08", "House", "Rules", type = "xml")

  expect_s3_class(d, "data.frame")
  expect_type(l, "list")
  expect_type(x[[1]], "externalptr")
})

test_that("function handles vector inputs", {
  comms <- c("Rules", "Judiciary")

  d <- getCommitteeMembers("2007-08", "House", comms, type = "df")
  l <- getCommitteeMembers("2007-08", "House", comms, type = "list")
  x <- getCommitteeMembers("2007-08", "House", comms, type = "xml")

  expect_equal(unique(d$CommitteeName), comms)
  expect_equal(names(l), comms)
  expect_equal(names(x), paste("2007-08", "House", comms, sep = "//"))
})

test_that("function checks for proper formatting", {
  expect_error(getCommitteeMembers("1989-90", "House", "Rules"))
  expect_error(getCommitteeMembers("2007-2008", "House", "Rules"))
})

test_that("function properly addresses capitalization", {
  x <- getCommitteeMembers("2007-08", "house", "Rules")
  y <- getCommitteeMembers("2007-08", "House", "Rules")

  expect_equal(x, y)
})
