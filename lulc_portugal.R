#packages

libs <- c(
  "terra",
  "giscoR",
  "sf",
  "tidyverse",
  "ggtern",
  "elevatr",
  "png",
  "rayshader",
  "magick"
)

installed_libraries <- libs %in% rownames(
  installed.packages()
)

if(any(installed_libraries == F)){
  
  install.packages(
    libs[!installed_libraries]
  )
}


invisible(
  lapply(
    libs, library, character.only = T
  )
)

# 2. Country borders

country_sf <- giscoR::gisco_get_countries(
  country = "PT"
)


# 3. Download ESRI Land Cover Tiles


library(raster)

str_name_1 <- '/path_to/29S_20220101-20230101.tif' 
str_name_2 <- '/path_to/29T_20220101-20230101.tif'
str_name_3 <- '/path_to/28S_20220101-20230101.tif'
str_name_4 <- '/path_to/25S_20220101-20230101.tif'
str_name_5 <- '/path_to/26S_20220101-20230101.tif'

imported_raster_1 <- raster(str_name_1)
imported_raster_2 <- raster(str_name_2)
imported_raster_3 <- raster(str_name_3)
imported_raster_4 <- raster(str_name_4)
imported_raster_5 <- raster(str_name_5)

raster_files <- list.files(
  path = "/path_to/files/",
  pattern = "tif",
  full.names = TRUE
)



crs <- "EPSG:32629"

for(raster in raster_files){
  rasters <- terra::rast(raster)
  
  country <- country_sf |>
    sf::st_transform(
      crs = terra::crs(
        rasters
      )
    )
  
  land_cover <- terra::crop(
    rasters,
    terra::vect(
      country
    ),
    snap = "in",
    mask = T
  ) |>
  terra::aggregate(
    fact = 5,
    fun = "modal"
  ) |>
  terra::project(crs)
  
  terra::writeRaster(
    land_cover,
    paste0(
      raster,
      "_portugal",
      ".tif"
    )
  )
}

# 5. Load Virtual Layer

r_list <- list.files(
  path = getwd(),
  pattern = "_portugal",
  full.names = T
    
)

land_cover_vrt <- terra::vrt(
  r_list,
  "portugal_land_cover_vrt.vrt",
  overwrite = T
)

# 6. Fetch Original Colors

ras  <- terra::rast(
  raster_files[[1]]
)

raster_color_table <- do.call(
  data.frame,
  terra::coltab(ras)
  
)

hex_code <- ggtern::rgb2hex(
  r = raster_color_table[,2],
  g = raster_color_table[,3],
  b = raster_color_table[,4]
)


# 7. Assign Colors to Raster
c("#000000", "#419bdf","#397d49" ,"#000000" ,"#7a87c6" ,"#e49635" ,"#000000" ,"#c4281b" ,"#a59b8f", "#a8ebff", "#616161", "#e3e2c3" )


cols <- hex_code[c(2:3, 5:6, 8:12)]

from <- c(1:2, 4:5, 7:11)
to <- t(col2rgb(cols))

land_cover_vrt <- na.omit(land_cover_vrt)

land_cover_portugal <- terra::subst(
  land_cover_vrt,
  from = from,
  to = to,
  names = cols
)

terra::plotRGB(land_cover_portugal)

# 8. Digital Elevation Model

elev <- elevatr::get_elev_raster(
  locations = country_sf,
  z = 4, clip = "locations"
)

plot(elev)

crs_lambert <-
  "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +datum=WGS84 +units=m +no_frfs"


land_cover_portugal_resampled <- terra::resample(
  x = land_cover_portugal,
  y = terra::rast(elev),
  method = "near"
)  |>
  terra::project(crs_lambert)

terra::plotRGB(land_cover_portugal_resampled)
print(land_cover_portugal_resampled)

img_file <- "land_cover_portugal.png"


terra::writeRaster(
  land_cover_portugal_resampled,
  img_file,
  overwrite =T,
  NAflag = 255
)

img <- png::readPNG(img_file)


# 9. Render Scene
#--------------------

elev_lambert <- elev |>
  terra::rast() |>
  terra::project(crs_lambert)

elmat <- rayshader::raster_to_matrix(
  elev_lambert
)

h <- nrow(elev_lambert)
w <- ncol(elev_lambert)

elmat |>
  rayshader::height_shade(
    texture = colorRampPalette(
      cols[9]
    )(256)
  ) |>
  rayshader::add_overlay(
    img,
    alphalayer = 1
  ) |>
  rayshader::plot_3d(
    elmat,
    zscale = 12,
    solid = F,
    shadow = T,
    shadow_darkness = 1,
    background = "white",
    windowsize = c(
      w/5, h/5
    ),
    zoom = .5,
    phi = 85, 
    theta = 0
  )


# Render Object

filename <- "3d_land_cover_portugal.png"

rayshader::render_highquality(
  filename = filename,
  preview = T,
  light = F,
  environment_light = '/path_to/air_museum_playground_4k.hdr',
  intensity_env = 1,
  rotate_env = 90,
  interactive = F,
  parallel = T,
  width = w * 1.5,
  height = h * 1.5
)


# 11. PUT EVERYTHING TOGETHER

c(
  "#419bdf", "#397d49", "#7a87c6", 
           "#e49635", "#c4281b", "#a59b8f", 
           "#a8ebff", "#616161", "#e3e2c3"
)

legend_name <- "land_cover_legend.png"
png(legend_name)
par(family = "mono")

plot(
  NULL,
  xaxt = "n",
  yaxt = "n",
  bty = "n",
  ylab = "",
  xlab = "",
  xlim = 0:1,
  ylim = 0:1,
  xaxs = "i",
  yaxs = "i"
)
legend(
  "center",
  legend = c(
    "Water",
    "Trees",
    "Crops",
    "Built area",
    "Rangeland"
  ),
  pch = 15,
  cex = 2,
  pt.cex = 1,
  bty = "n",
  col = c(cols[c(1:2, 4:5, 9)]),
  fill = c(cols[c(1:2, 4:5, 9)]),
  border = "grey20"
)
dev.off()

# filename <- "land-cover-bih-3d-b.png"

lc_img <- magick::image_read(
  filename
)

my_legend <- magick::image_read(
  legend_name
)

my_legend_scaled <- magick::image_scale(
  magick::image_background(
    my_legend, "none"
  ), 2500
)

p <- magick::image_composite(
  magick::image_scale(
    lc_img, "x7000" 
  ),
  my_legend_scaled,
  gravity = "southwest",
  offset = "+100+0"
)

magick::image_write(
  p, "3d_portugal_land_cover_final.png"
)














