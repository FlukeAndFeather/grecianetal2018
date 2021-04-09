## code to prepare `all_tracks_df` dataset goes here
adu_tracks_df <- "analysis/data/raw_data/gannet_gps_locs_2015_adults.csv" %>%
  readr::read_csv() %>%
  dplyr::mutate(age = "adult")
imm_tracks_df <- "analysis/data/raw_data/gannet_gps_locs_2015_immatures.csv" %>%
  readr::read_csv() %>%
  dplyr::mutate(age = "immature")
all_tracks_df <- rbind(adu_tracks_df, imm_tracks_df)

usethis::use_data(all_tracks_df, overwrite = TRUE)
