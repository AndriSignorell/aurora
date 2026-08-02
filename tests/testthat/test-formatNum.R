
test_that("formatNum_cpp works", {
  x <- c(1.2345, 1000.2)
  expect_type(formatNum_cpp(x), "character")
})

