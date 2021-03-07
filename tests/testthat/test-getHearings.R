test_that("getHearings returns nonempty table", {
  expect_gt(length(getHearings("2007-08", "1001")), 0)
})
