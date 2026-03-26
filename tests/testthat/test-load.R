

test_that("aurora does not load without DescToolsX", {
  expect_error(
    library(aurora),
    NA
  )
})
