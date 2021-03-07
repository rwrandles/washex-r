test_that("getLegislation returns a nonempty dataframe", {
  expect_type(getLegislationByYear("2007"), "list")
  expect_type(getLegislationByYear(2007), "list")
  expect_gt(nrow(getLegislationByYear("2007")), 0)
})
