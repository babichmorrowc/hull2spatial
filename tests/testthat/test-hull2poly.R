test_that("ahull2poly example return spatialpolygon", {
  iris_sepals <- iris[,1:2]
  iris_sepals <- iris_sepals[!duplicated(paste(iris_sepals$Sepal.Length, iris_sepals$Sepal.Width)), ]
  alphahull_1 <- alphahull::ahull(iris_sepals, alpha = 1)
  hullpoly_1 <- ahull2poly(alphahull_1)
  expect_s4_class(hullpoly_1, "SpatialPolygons")
})
