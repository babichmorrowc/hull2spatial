test_that("example return spatialpolygons", {
  set.seed(123)
  x <- matrix(runif(100), nc = 2)
  ashape_02 <- alphahull::ashape(x, alpha = 0.2)
  poly_result <- ashape2poly(ashape_02)
  expect_s4_class(poly_result, "SpatialPolygons")
})

test_that("not connected error", {
  set.seed(123)
  x <- matrix(runif(100), nc = 2)
  ashape_005 <- alphahull::ashape(x, alpha = 0.05)
  expect_error(ashape2poly(ashape_005), "not connected")
})

