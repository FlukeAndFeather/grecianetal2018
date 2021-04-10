library(targets)

# Set target-specific options such as packages.
pkgs <- strsplit(utils::packageDescription("grecianetal2018",
                                           fields = "Imports"),
                 ",\n")[[1]]
tar_option_set(packages = c(pkgs, "grecianetal2018"))

# End this file with a list of target objects.
list(
  # Utilization distributions and home range estimates
  tar_target(noga_uds, util_dist(noga_tracks)),
  tar_target(noga_hrs, home_range(noga_uds)),
  # Geographic objects
  tar_target(land, get_naturalearth("land")),
  tar_target(islands, get_naturalearth("minor_islands")),
  tar_target(bathy, get_bathy(extent(noga_hrs),
                              extent_mult = 5,
                              bathy_levels = c(-50, -150, -250))),
  # Make figure 1 and save to file
  tar_target(fig1, make_fig1(noga_hrs, land, islands, bathy), format = "file"),

  # Observed and null Bhattacharyya's affinity for 50% and 95% UD home ranges
  tar_target(obs_ba, observed_ba(noga_uds)),
  # Note: this step runs really long; choose n_cores according to your system
  tar_target(null_ba, sim_null_ba(n_cores = 4)),
  tar_target(fig2, make_fig2(obs_ba, null_ba), format = "file")
)
