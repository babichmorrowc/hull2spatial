test_that("arc2line example return line", {
  example_result <- arc2line(center = c(0.5, -0.1), r = 0.2, vector = c(0.1, 1), theta = 0.04)
  expect_s4_class(example_result, "Line")
})

test_that("ahull2lines example return spatiallines", {
  set.seed(123)
  x <- matrix(runif(100), nc = 2)
  ahull_02 <- alphahull::ahull(x, alpha = 0.2)
  ahull_line_02 <- ahull2lines(ahull_02)
  expect_s4_class(ahull_line_02, "SpatialLines")
})
