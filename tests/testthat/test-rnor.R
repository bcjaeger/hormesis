
test_that("rnor works as intended", {
  
  values <- rnor(n = 100, mean = 3, sd = 1, low_threshold = 2)
  
  expect_true(all(values >= 2))
  expect_true(length(values) == 100)
  
})
