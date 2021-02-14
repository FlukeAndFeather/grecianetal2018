library(adehabitatHR)
library(raster)
library(rnaturalearth)
library(sf)
library(tidyverse)

# Load track data
adu_tracks_df <- "analysis/data/raw_data/gannet_gps_locs_2015_adults.csv" %>%
  read_csv() %>%
  mutate(age = "adult")
imm_tracks_df <- "analysis/data/raw_data/gannet_gps_locs_2015_immatures.csv" %>%
  read_csv() %>%
  mutate(age = "immature")
all_tracks_df <- rbind(adu_tracks_df, imm_tracks_df)

# Have points already been regularized?
all_tracks_df %>%
  group_by(trip) %>%
  mutate(dt = as.numeric(lead(datetime_utc) - datetime_utc, unit = "mins")) %>%
  ungroup() %>%
  summarize_at(vars(dt), list(min, max, mean), na.rm = TRUE)
# Looks good!
# # A tibble: 1 x 3
#     fn1   fn2   fn3
#   <dbl> <dbl> <dbl>
# 1     2     2     2

# adehabitatHR::kernelUD() takes SpatialPoints objects
# run kernelUD() once (instead of per-age class) so results end up on same grid

# UTM 30N crs
utm_30n <- CRS("+init=EPSG:32630")

uds <- all_tracks_df %>%
  {SpatialPointsDataFrame(coords = cbind(.$UTM_x, .$UTM_y),
                          data = data.frame(id = .$age),
                          proj4string = utm_30n)} %>%
  # Methods specify 1 km grid, 10 km smoothing parameter
  kernelUD(grid = 1e3, h = 10e3)

ud_levels <- c(25, 50, 75, 95)
ud_to_sf <- function(percent) {
  getverticeshr(uds, percent) %>%
    st_as_sf() %>%
    mutate(ud_level = paste0(percent, "%"))
}
hrs <- map_dfr(ud_levels, ud_to_sf) %>%
  rename(age = id) %>%
  mutate(ud_level = factor(ud_level, levels = c("95%", "75%", "50%", "25%")))


# Land shapefiles
fig1_extent <- extent(hrs) * 1.2
land_sf <- ne_download(scale = "large",
                       type = "land",
                       category = "physical",
                       returnclass = "sf") %>%
  st_transform(utm_30n)

# Fig 1a
ggplot() +
  geom_sf(data = land_sf, fill = "#989898", color = NA) +
  geom_sf(aes(fill = ud_level), data = hrs, color = NA) +
  scale_x_continuous(breaks = c(-5, 0, 5)) +
  scale_y_continuous(breaks = seq(52, 60, by = 2)) +
  scale_fill_manual(values = c(`25%` = "#DB602B",
                               `50%` = "#E9C14C",
                               `75%` = "#FFFF5A",
                               `95%` = "#D5D5D5")) +
  labs(fill = "UD") +
  guides(fill = guide_legend(reverse = TRUE)) +
  coord_sf(xlim = fig1_extent[1:2],
           ylim = fig1_extent[3:4]) +
  facet_grid(cols = vars(age)) +
  theme_minimal()
