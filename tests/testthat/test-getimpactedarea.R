test_that("getimpactedarea", {
  fac_tehri <- rast(system.file("extdata", "fac_tehri.tif", package="damaoi"))
  dem_tehri <- rast(system.file("extdata", "dem_tehri.tif", package="damaoi"))
  wb_tehri <- rast(system.file("extdata", "wb_tehri.tif", package="damaoi"))
  reservoir = tehri;

aoi <- getimpactedarea(
  reservoir = tehri,
  water_bodies = wb_tehri,
  dem = dem_tehri,
  fac = fac_tehri,
  basins = basins_tehri,
  tocrop = F,
  poss_expand = 10000,
  river_distance = 10000,
  nn = 100,
  ac_tolerance = 2,
  e_tolerance = 5,
  streambuffersize = 2000,
  reservoirbuffersize = 5000,
  wbjc = 0
)
expect_equal(nrow(aoi), 3)
expect_equal(sum(class(aoi) == "sf"),1)
})
