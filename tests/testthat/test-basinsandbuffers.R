test_that("basinsandbuffers", {
  tehri_fac <- rast(system.file("extdata", "fac_tehri.tif", package="damaoi"))
  tehri_dem <- rast(system.file("extdata", "dem_tehri.tif", package="damaoi"))
  tehri_wb <- rast(system.file("extdata", "wb_tehri.tif", package="damaoi"))
  tehri_adjusted <- adjustreservoirpolygon(tehri, tehri_wb, tehri_dem, 20000, 0)
  up <- getriverpoints(reservoir = tehri, 
                       direction = "upstream", 
                       river_distance = 10000,
                       ac_tolerance = 50,
                       e_tolerance = 10, 
                       nn = 100, 
                       fac = tehri_fac,
                       dem = tehri_dem)
  down <- getriverpoints(reservoir = tehri, 
                       direction = "upstream", 
                       river_distance = 10000,
                       ac_tolerance = 50,
                       e_tolerance = 10, 
                       nn = 100, 
                       fac = tehri_fac,
                       dem = tehri_dem)
  bnb <- basinandbuffers(
    reservoir = tehri_adjusted,
                          upstream = up[[2]],
                          downstream = down[[2]],
                          basins = basins_tehri,
                          streambuffersize = 1500,
                          reservoirbuffersize = 3000)
  expect_equal(sum(class(bnb) == "sf"),1)
  expect_equal(st_area(bnb) > st_area(tehri_adjusted), TRUE)
})
