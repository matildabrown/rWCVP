## code to prepare `wgsrpd3` and derived datasets goes here
library(sf)
library(lwgeom)
library(dplyr)

# wgsrpd3 ----
shp_url <- "https://github.com/tdwg/wgsrpd/archive/refs/heads/master.zip"

# download and extract the folder
temp <- tempfile()
download.file(shp_url, temp)
unzip(temp)

# load level 3 shape file
wgsrpd3_orig <- st_read("wgsrpd-master/level3/level3.shp")

# clean up directory
unlink(temp)
unlink("wgsrpd-master", recursive=TRUE)

# before fixing the shape file doesn't work with spherical coordinates
sf_use_s2(FALSE)

wgsrpd3 <- st_buffer(wgsrpd3_orig, 0)
wgsrpd3 <- st_crop(wgsrpd3, st_bbox(c(xmin=-180, xmax=180, ymin=-90, ymax=90)))

usethis::use_data(wgsrpd3, overwrite=TRUE)
