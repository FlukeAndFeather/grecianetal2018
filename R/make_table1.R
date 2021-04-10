# Trip utility functions
trip_hours <- function(dt) {
  as.numeric(last(dt) - first(dt), units = "hours")
}
trip_length <- function(x, y) {
  geodist::geodist(x = cbind(lon = x, lat = y),
                   sequential = TRUE,
                   measure = "geodesic") %>%
    max() %>%
    {. / 1000}
}
trip_angle <- function(x, y) {
  x5 <- mean(x[1:5])
  y5 <- mean(y[1:5])
  atan(y5 / x5)
}
trip_range <- function(x, y) {
  geodist::geodist(x = cbind(lon = -2.60, lat = 57.0),
                   y = cbind(lon = x, lat = y),
                   measure = "geodesic") %>%
    max() %>%
    {. / 1000}
}
trip_area <- function(x, y) {
  sp::SpatialPoints(cbind(x, y), proj4string = utm_30n) %>%
    adehabitatHR::mcp.area(percent = 100,
                           unin = "m", unout = "km2",
                           plotit = FALSE) %>%
    unlist()
}

#' Summarize trips
#'
#' @return A data with 247 rows and 8 variables: age category, individual id,
#'   trip id, trip duration (hours), trip departure angle (radians), trip length
#'   (km), trip range (km), and trip area (km^2)
#' @export
trip_summaries <- function() {
  noga_tracks %>%
    group_by(age, id, trip) %>%
    summarize(duration_h = trip_hours(datetime_utc),
              trip_rad = trip_angle(UTM_x, UTM_y),
              length_km = trip_length(lon, lat),
              range_km = trip_range(lon, lat),
              area_km2 = trip_area(UTM_x, UTM_y),
              .groups = "drop")
}

# Likelihood ratio tests for trip parameters by age category
trip_likrat_tests <- function(trips) {
  trip_lmm <- function(lhs) {
    trip_lmm1 <- lme4::lmer(as.formula(paste(lhs, "age + (1|id)", sep = "~")),
                            data = trips,
                            REML = FALSE)
    trip_lmm2 <- update(trip_lmm1, . ~ (1|id))
    trip_anova <- anova(trip_lmm1, trip_lmm2)
    result <- trip_anova[2, c("Chisq", "Pr(>Chisq)")] %>% unlist()
    names(result) <- c("chisq", "p")
    result
  }

  sapply(c("log10(duration_h)",
           "log10(length_km)",
           "range_km",
           "log10(area_km2)"),
         trip_lmm) %>%
    t()
}

#' Make table 1
#'
#' Table 1 is a summary of foraging trip metrics
#'
#' @param trips see `trip_summaries`
#'
#' @return a data frame with four rows (trip metrics: duration, length, range,
#'   and area) and 9 columns (metric name; median, min, and max of metric by age
#'   category; chisquared statistic and p-value for likelihood ratio test).
#' @export
make_table1 <- function(trips) {
  trips %>%
    group_by(age) %>%
    summarize(across(duration_h:area_km2,
                     list(med = median, min = min, max = max),
                     .names = "{.col}-{.fn}"),
              .groups = "drop") %>%
    pivot_longer(-age) %>%
    pivot_wider(names_from = age) %>%
    separate(name, into = c("parameter", "fn"), sep = "-") %>%
    pivot_wider(names_from = fn, values_from = adult:immature) %>%
    filter(parameter != "trip_rad") %>%
    cbind(trip_likrat_tests(trips))
}
