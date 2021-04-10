#' Observed home range overlap between adult and immature Northern Gannets
#'
#' @param uds Northern Gannet utilization distribution (see `util_dist`)
#'
#' @return A data frame with variables: percent (50%, 95%) and ba
#'   (Bhattacharyya's affinity for 50% and 95% UD home ranges)
#' @export
observed_ba <- function(uds) {
  obs_ba_50 <- kerneloverlaphr(uds,
                               method = "BA",
                               percent = 50,
                               conditional = TRUE)["adult", "immature"]
  obs_ba_95 <- kerneloverlaphr(uds,
                               method = "BA",
                               percent = 95,
                               conditional = TRUE)["adult", "immature"]
  data.frame(percent = c("50%", "95%"),
             ba = c(obs_ba_50, obs_ba_95))
}

#' Null distribution of home range overlap between adult and immature Northern
#' Gannets
#'
#' Calculated by shuffling adult and immature age assignments.
#'
#' @param n_cores Number of cores
#'
#' @return A data frame with variables: percent (50%, 95%), ba (Bhattacharyya's
#'   affinity for 50% and 95% UD home ranges), and replicate (1:1000)
#' @export
sim_null_ba <- function(n_cores = 1) {
  set.seed(1331)
  N <- 4
  replicates <- parallel::mclapply(1:N, function(x) {
    shuffled <- noga_tracks %>%
      distinct(id, age) %>%
      mutate(age = sample(age, length(age), replace = FALSE))
    uds <- noga_tracks %>%
      select(-age) %>%
      left_join(shuffled, by = "id") %>%
      util_dist()
    c(ba_50 = adehabitatHR::kerneloverlaphr(uds,
                                            method = "BA",
                                            percent = 50,
                                            conditional = TRUE)["adult", "immature"],
      ba_95 = adehabitatHR::kerneloverlaphr(uds,
                                            method = "BA",
                                            percent = 95,
                                            conditional = TRUE)["adult", "immature"])

  }, mc.cores = 4)
  data.frame(replicate = c(1:N, 1:N),
             percent = rep(c("50%", "95%"), each = N),
             ba = c(purrr::map_dbl(replicates, "ba_50"),
                    purrr::map_dbl(replicates, "ba_95")))
}
