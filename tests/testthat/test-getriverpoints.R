test_that("getriverpoints_checks", {
  tehri_fac <- rast(system.file("extdata", "fac_tehri.tif", package="damaoi"))
  tehri_dem <- rast(system.file("extdata", "dem_tehri.tif", package="damaoi"))
  up <- getriverpoints(reservoir = tehri, 
                       direction = "upstream", 
                       river_distance = 10000,
                       ac_tolerance = 50,
                       e_tolerance = 10, 
                       nn = 100, 
                       fac = tehri_fac,
                       dem = tehri_dem)
  expect_equal(class(up[[1]]), "data.frame")
  expect_equal(ncol(up[[1]]), 7)
  upline <- up[[2]]
  expect_equal(as.character(droplevels(st_geometry_type(upline))), "LINESTRING")
  })
