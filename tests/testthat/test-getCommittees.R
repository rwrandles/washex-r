test_that("function works for each input type", {
  d <- getCommittees("2007-08", type = "df")
  l <- getCommittees("2007-08", type = "list")
  x <- getCommittees("2007-08", type = "xml")

  expect_s3_class(d, "data.frame")
  expect_type(l, "list")
  expect_type(x[[1]], "externalptr")
})

test_that("function handles vector inputs", {
  bienns <- c("2007-08", "2009-10")

  d <- getCommittees(bienns, type = "df")
  l <- getCommittees(bienns, type = "list")
  x <- getCommittees(bienns, type = "xml")

  expect_equal(unique(d$Biennium), bienns)
  expect_equal(names(l), bienns)
  expect_equal(names(x), bienns)
})

test_that("function checks for proper formatting", {
  expect_error(getCommittees("2007-2008"))
  expect_error(getCommittees("1989-90"))
})
