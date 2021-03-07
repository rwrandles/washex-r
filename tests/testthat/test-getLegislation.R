test_that("getLegislation() class checks", {
  expect_type(getLegislation("2007-08", "1001"), "list")
  expect_type(getLegislation("2007-08", 1001), "list")
  expect_type(getLegislation("2007-08", "1001", as.xml = TRUE), "externalptr")
})
