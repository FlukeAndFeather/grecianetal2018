library(rnaturalearth)
library(sf)
library(tidyverse)

adu_tracks_df <- "analysis/data/raw_data/gannet_gps_locs_2015_adults.csv" %>%
  read_csv() %>%
  mutate(age = "adult")
imm_tracks_df <- "analysis/data/raw_data/gannet_gps_locs_2015_immatures.csv" %>%
  read_csv() %>%
  mutate(age = "immature")
all_tracks_df <- rbind(adu_tracks_df, imm_tracks_df)

coords_to_lines <- function(x, y) {
  stopifnot(
    is.numeric(x),
    is.numeric(y),
    length(x) == length(y)
  )

  list(st_linestring(cbind(x, y)))
}

all_tracks_sf <- all_tracks_df %>%
  group_by(id, trip, age) %>%
  summarize(geometry = coords_to_lines(lon, lat),
            .groups = "drop") %>%
  st_as_sf(crs = "EPSG:4326")

land50 <- ne_download(scale = 50,
                      type = "land",
                      category = "physical",
                      returnclass = "sf") %>%
  st_crop(xmin = -5,
          ymin = 50,
          xmax = 10,
          ymax = 65)

ggplot(all_tracks_sf) +
  geom_sf(data = land50,
          fill = "grey60",
          color = NA) +
  geom_sf(aes(color = age)) +
  coord_sf(xlim = st_bbox(all_tracks_sf)[c("xmin", "xmax")],
           ylim = st_bbox(all_tracks_sf)[c("ymin", "ymax")]) +
  facet_grid(cols = vars(age))
