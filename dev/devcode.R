rm(list = ls())
library(terra)
library(whitebox)
devtools::load_all()

wb_workspace <- function(prefix = "wb") {
  path <- file.path(tempdir(), paste0(prefix, "_", as.integer(Sys.time())))
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  return(normalizePath(path, winslash = "/"))
}
ws <- wb_workspace()

tehri <- vect(damAOI::tehri)
dem_tehri <- rast("./inst/extdata/dem_tehri.tif")

ll <- xyFromCell(dem_tehri, cellFromRowCol(dem_tehri, nrow(dem_tehri)/2, ncol(dem_tehri)/2))
espg <- getutm(longitude = ll[1], latitude = ll[2])
dem_tehri <- project(dem_tehri, st_crs(espg)[[2]])
tehri <- project(tehri, dem_tehri)

writeRaster(dem_tehri, paste0(ws, "/dem.tif"))

dem_conditioned <- wbt_breach_depressions(
  paste0(ws, "/dem.tif"),
  output = paste0(ws, "/dc.tif"))

wbt_d8_flow_accumulation(
  i = paste0(ws, "/dc.tif"),
  output = paste0(ws, "/facc.tif"))

wbt_extract_streams(
  flow_accum = paste0(ws, "/facc.tif"),
  output = paste0(ws, "/streams.tif"),
  threshold = 1000
)

wbt_d8_pointer(
  dem = paste0(ws, "/dc.tif"),
  output = paste0(ws, "/fdir.tif"))

riv <- rast(paste0(ws, "/streams.tif"))
riv4326 <- project(riv, st_crs(4326)[[2]])
res4326 <- st_as_sf(project(tehri, st_crs(4326)[[2]]))
coltab(riv4326) <- data.frame(value = 1, color = "#FE019A")
app <- getshinyparams(res = res4326, streams = riv4326)

pourpoints <- runApp(shinyApp(app$ui, app$server))


pourpoints

ppin <- terra::buffer(as.points(rast(paste0(ws, "/pp_in.tif"))), width = res(fac)[[1]]*2)

plot(densify(ppin, 30))

?wbt_watershed
?buffer
writeVector(ppin, paste0(ws,"/ppin.shp"), overwrite = TRUE)

wbt_watershed(
  d8_pntr = paste0(ws, "/fdir.tif"),
  pour_pts = paste0(ws, "/pp_in.tif"),
  output = paste0(ws, "/ws_up.tif"),
  verbose_mode =TRUE
)

list.files(ws)

plot(dem_tehri)
plot(tehri, add = TRUE)
plot(rast(paste0(ws, "/ws_up.tif")), add = TRUE)
plot()

getwd()
list.files(ws)
test <- rast(paste0(ws, "/ww_up.tif"))
ws
list.files(ws)
reservoir <- tehri  
  
  

getpourpoints <- function(reservoir, ws){

dem <- rast(paste0(ws, "/dem.tif")) 
res_rast <- rasterize(reservoir, dem, field = 1, background = NA)
res_bound <- boundaries(res_rast, inner = FALSE, falseval = NA)
streams <- rast(paste0(ws, "/streams.tif"))
inter <- streams*res_bound
icells <- cells(inter)
fdir <- rast(paste0(ws, "/fdir.tif"))
fd <- values(fdir)[icells]

rc <- rowColFromCell(fdir, icells)

idx <- match(fd, c(1,2,4,8,16,32,64,128))

dr <- c(0,1,1,1,0,-1,-1,-1)[idx]
dc <- c(1,1,0,-1,-1,-1,0,1)[idx]
ds <- cellFromRowCol(fdir, rc[,1] + dr, rc[,2] + dc)

lake_next <- !is.na(values(tr)[ds])

outflow <- !lake_next

pourpoint_raster <- rast(dem)
pourpoint_raster[] <- fac
pourpoint_raster[icells[outflow]] <- 0
pourpoint_raster[icells[!outflow]] <- 1
plot(trim(pourpoint_raster))
plot(writeRaster())
fac <- rast(paste0(ws, "/facc.tif"))
plot(tehri)
plot(rast(paste0(ws, "/pp_in.tif")), add = TRUE)
plot(streams)
list.files(ws)
pourpoint_raster[icells[which.max(values(fac)[icells])]] <- 0
pourpoint_raster[icells[which.min(values(fac)[icells])]] <- 1

ppr_in <- pourpoint_raster
ppr_out <- pourpoint_raster
ppr_in[ppr_in != 1] <- NA
ppr_out[ppr_out != 0] <- NA

pp <- as.points(pourpoint_raster)
names(pp) <- "direction"
ppin <- pp[pp$direction == 1,]
ppout <- pp[pp$direction == 0,]
writeRaster(ppr_in, paste0(ws, "/pp_in.tif"))
writeRaster(ppr_out, paste0(ws, "/pp_out.tif"))
return(pourpoint_raster)
}

wbt_watershed(
  d8_pntr = "fdir.tif",
  pour_pts = "point_snapped.shp",
  output = "watershed.tif"
)

start <- extract(fd, buffer(vect(pourpoints[1,]), res(fd)[[1]]*1), cells = TRUE)$cell
seed <- rast(fd)
seed <- project(seed, rast("./dev/dc.tif"))
seed <- writeRaster(seed, "./dev/seed.tif")


upstream_streams <- writeRaster(
  rast("./dev/streams.tif") * rast("./dev/upstream_area.tif"),
                                "./dev/upstream_streams.tif")
wbt_raster_calculator(
  input1 = "./dev/streams.tif",
  input2 = "./dev/upstream_area.tif",
  output = "./dev/upstream_streams.tif",
  formula = "i1 * i2"
)

wbt_extract_streams(
  flow_accum = "./dev/facc.tif",
  output = "./dev/streams_up.tif",
  threshold = 1000
)
wbt_length_of_upstream_channels(
  d8_pntr = "./dev/fdir.tif",
  streams = "./dev/upstream_streams.tif",
  output = "./dev/upstream_channel_length.tif"
)

fl <- wbt_d8_flow_length(
  d8_pntr = "./dev/fdir.tif",
  output = "./dev/flow_length.tif",
  is_downslope = FALSE
)



test <- rast("./dev/upstream_area.tif")
plot(test)
writeRaster()
seed[] <- NA
seed[start] <- 1
plot(seed, add = TRUE)
plot(fd)
tpl <- rast(fd)
tpl[] <- NA
plot(fd)
tpl[test] <- 1
plot(tpl)
plot(test)

reservoir = tehri_adjusted;
pourpoints = pourpoints;
river_distance = 30000;
ac_tolerance = 50;
e_tolerance = 10;
nn = 20;
fac = tehri_fac_utm;
dem = tehri_dem_utm

riverlines <- lapply(
  X = 1:2,
  FUN = getriverpointsgraph,
  reservoir = tehri_adjusted,
  pourpoints = pourpoints,
  river_distance = 60000,
  ac_tolerance = 50,
  e_tolerance = 10,
  nn = 20,
  fac = tehri_fac_utm,
  dem = tehri_dem_utm,
  branch_min_ac = 1000,
  branch_ratio_min = 0.02,
  branch_persistence_distance = 5000,
  branch_fac_ratio = 30
)

get_bearing <- function(geom) {
  coords <- st_coordinates(geom)
  p1 <- coords[1, ]
  p2 <- coords[nrow(coords), ]
  (atan2(p2[2] - p1[2], p2[1] - p1[1]) * 180 / pi) %% 180
}

test <- st_as_sf(riverlines[[1]])

nrow(riverlines[[1]])
riverlines[[1]]$bearing <- sapply(st_geometry(test), get_bearing)
coords <- st_centroid(test) |> st_coordinates()

riverlines[[1]] <- cbind(riverlines[[1]], st_centroid(test) |> st_coordinates())
test <- riverlines[[1]]


values(test)

tc <- scale(values(test[,1:3]))
tc

cl <- dbscan(tc, eps = 0.1, minPts = 3)

test$c <- kmeans(tc, 2)$cluster

plot(test, "c")
install.packages("dbscan")
library(dbscan)
X <- cbind(
  coords,
  sin(s$bearing * pi/180),
  cos(s$bearing * pi/180)
)

test <- st_as_sf(riverpoints[[1]], res(tehri_fac_utm))
