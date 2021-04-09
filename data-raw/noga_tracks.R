## code to prepare `noga_tracks` dataset goes here
adu_tracks_df <- "analysis/data/raw_data/gannet_gps_locs_2015_adults.csv" %>%
  readr::read_csv() %>%
  dplyr::mutate(age = "adult")
imm_tracks_df <- "analysis/data/raw_data/gannet_gps_locs_2015_immatures.csv" %>%
  readr::read_csv() %>%
  dplyr::mutate(age = "immature")
noga_tracks <- rbind(adu_tracks_df, imm_tracks_df)

usethis::use_data(noga_tracks, overwrite = TRUE)
