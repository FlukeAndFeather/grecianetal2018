#' Retrieve Natural Earth data
#'
#' @param ne_type Type of Natural Earth data e.g. "land" or "minor islands"
#'
#' @return An `sf` object
#' @export
get_naturalearth <- function(ne_type) {
  rnaturalearth::ne_download(scale = "large",
                             type = ne_type,
                             category = "physical",
                             returnclass = "sf") %>%
    sf::st_transform(utm_30n)
}

#' Retrieve bathymetric data
#'
#' @param utm30n_extent An `extent` object in UTM 30N coordinates (e.g. the
#'   extent of the Northern Gannet tracks)
#' @param extent_mult A multiplier applied to the extent (e.g. 5)
#' @param bathy_levels A vector of bathymetric contour levels (e.g. `c(-50, -150,
#'   -250)`)
#'
#' @return An `sf` object of the bathymetric contours
#' @export
get_bathy <- function(utm30n_extent, extent_mult, bathy_levels) {
  bathy_extent <- (utm30n_extent * extent_mult) %>%
    raster::raster(crs = utm_30n) %>%
    raster::projectExtent("+proj=longlat +datum=WGS84") %>%
    raster::extent()
  marmap::getNOAA.bathy(lon1 = bathy_extent[1],
                        lon2 = bathy_extent[2],
                        lat1 = bathy_extent[3],
                        lat2 = bathy_extent[4],
                        resolution = 1) %>%
    marmap::as.raster() %>%
    raster::rasterToContour(levels = bathy_levels) %>%
    sf::st_as_sf() %>%
    mutate(depth = factor(level,
                          levels = bathy_levels,
                          labels = paste(bathy_levels, "m")))
}
