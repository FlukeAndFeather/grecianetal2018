#' Utilization distribution of tracks
#'
#' @param tracks Use `all_tracks_df` for the actual data, or shuffle ages to generate null Bhattacharyya's affinity distributions
#'
#' @return A list of two `estUD` objects: adult and immature
#' @export
util_dist <- function(tracks) {
  tracks %>%
    {sp::SpatialPointsDataFrame(coords = cbind(.$UTM_x, .$UTM_y),
                                data = data.frame(id = .$age),
                                proj4string = utm_30n)} %>%
    # Methods specify 1 km grid, 10 km smoothing parameter
    adehabitatHR::kernelUD(grid = 1e3, h = 10e3, same4all = TRUE)
}

#' Extract home ranges at 25%, 50%, 75%, and 95% utilization distribution levels
#'
#' @param uds Adult and immature utilization distributions
#'
#' @return An sf data.frame with columns: age ("adult" or "immature"), area,
#'   ud_level (a factor: 25%, 50%, 75%, and 95%), and geometry,
#' @export
home_range <- function(uds) {
  ud_to_sf <- function(percent) {
    adehabitatHR::getverticeshr(uds, percent) %>%
      sf::st_as_sf() %>%
      mutate(ud_level = paste0(percent, "%"))
  }
  ud_levels <- c(25, 50, 75, 95)
  map_dfr(ud_levels, ud_to_sf) %>%
    rename(age = id) %>%
    mutate(ud_level = factor(ud_level, levels = c("95%", "75%", "50%", "25%")))
}
