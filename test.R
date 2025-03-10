# install.packages("remotes")
remotes::install_github("crazycapivara/h3-r")
library(h3)

pop_sf
#Simple feature collection with 373545 features and 2 fields
#Geometry type: POLYGON
#Dimension:     XY
#Bounding box:  xmin: 4030556 ymin: 2683864 xmax: 4672934 ymax: 3551525
#Projected CRS: +proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +datum=WGS84 +units=m +no_defs
#First 10 features:
#  h3                  population         geom
#  1  881fa3784bfffff         27       POLYGON ((4072246 2922265, ...
#  2  881fa37849fffff         11       POLYGON ((4071382 2922427, ...
#  3  881fa37845fffff         34       POLYGON ((4070958 2920956, ...
#  4  881fa37843fffff          8       POLYGON ((4072466 2921448, ...
#  5  881fa37809fffff        515       POLYGON ((4072687 2920632, ...
                      
countries<- geodata::gadm(
  country = c("AND"),level = 1, path = getwd() ) %>%
  sf::st_as_sf() %>%
  sf::st_cast("MULTIPOLYGON") %>%  st_transform(crs = st_crs(crsLAEA))

andorra <- readRDS("Iberia_AD.rds")
andorra <- andorra[,c(1,7,8)]
andorra_sf <- st_as_sf(andorra, coords = c("decimalLongitude", "decimalLatitude"), crs = crsLAEA)

library(h3r)
library(h3jsr)
library(h3lib)

andorra$h3 <- latLngToCell(lat = andorra$decimalLatitude,
                              lng = andorra$decimalLongitude ,res = 10)

coords <- st_coordinates(andorra_sf)
andorra$h3 <- geo_to_h3(coords, res = 12)

unique(andorra$h3)

andorra_h3 <- andorra %>%
  group_by(h3) %>%
  summarise(species_count = n_distinct(species))

andorra_h3$geom<- h3_to_geo_boundary_sf(andorra_h3$h3)

andorra_h3$geom <- st_transform(andorra_h3$geom)
names(andorra_h3)

andorra_h3 <- andorra_h3 %>%
  mutate(geom = st_as_sfc(geom)) %>%
  st_as_sf(crs = st_crs(pop_sf))

andorra_h3 <- st_as_sf(andorra_h3$geom,  crs = st_crs(pop_sf))
andorra_h3 <- st_transform(andorra_h3, st_crs(pop_sf))

ggplot() +
  geom_sf(data = andorra_h3,color = "grey10", fill = "grey10")



bb <- sf::st_bbox(andorra_h3)

get_raster_size <- function() {
  height <- sf::st_distance(
    sf::st_point(c(bb[["xmin"]], bb[["ymin"]])),
    sf::st_point(c(bb[["xmin"]], bb[["ymax"]]))
  )
  width <- sf::st_distance(
    sf::st_point(c(bb[["xmin"]], bb[["ymin"]])),
    sf::st_point(c(bb[["xmax"]], bb[["ymin"]]))
  )
  
  if (height > width) {
    height_ratio <- 5
    width_ratio <- width / height
  } else {
    width_ratio <- 5
    height_ratio <- height / width
  }
  
  return(list(width_ratio, height_ratio))
}
width_ratio <- get_raster_size()[[1]]
height_ratio <- get_raster_size()[[2]]

size <- 3000
width <- round((size * width_ratio), 0)
height <- round((size * height_ratio), 0)

get_population_raster <- function() {
  pop_rast <- stars::st_rasterize(
    andorra_h3 |>
      dplyr::select(species_count, geom),
    nx = width, ny = height
  )
  
  return(pop_rast)
}

pop_rast <- get_population_raster()

pop_mat <- pop_rast |>
  as("Raster") |>
  rayshader::raster_to_matrix()

cols <- rev(c(
  "#0b1354", "#283680",
  "#6853a9", "#c863b3"
))

texture <- grDevices::colorRampPalette(cols)(256)

# Create the initial 3D object
pop_mat |>
  rayshader::height_shade(texture = texture) |>
  rayshader::plot_3d(
    heightmap = pop_mat,
    solid = F,
    soliddepth = 0,
    zscale = 15,
    shadowdepth = 0,
    shadow_darkness = .95,
    windowsize = c(800, 800),
    phi = 65,
    zoom = .65,
    theta = -30,
    background = "white")



# Use this to adjust the view after building the window object
rayshader::render_camera(phi = 75, zoom = .7, theta = 0)

rayshader::render_highquality(
  filename = "germany_population_2022.png",
  preview = T,
  light = T,
  lightdirection = 225,
  lightaltitude = 60,
  lightintensity = 400,
  interactive = F,
  width = width, height = height
)
