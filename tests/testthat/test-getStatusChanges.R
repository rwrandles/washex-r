test_that("Status changes are successfully retrieved", {
  expect_gt(nrow(getStatusChanges("2007-08", "1001")), 0)
})
