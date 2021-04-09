#' Make figure 1
#'
#' Figure 1 is a map comparing the home ranges of adult and immature Northern
#' Gannets.
#'
#' @param home_ranges Northern Gannet home ranges (see `home_range`)
#' @param land `sf` object with land geometries (see `get_naturalearth`)
#' @param islands `sf` object with minor island geometries (see
#'   `get_naturalearth`)
#' @param bathy `sf` object with bathymetric contours (see `get_bathy`)
#'
#' @return `ggplot` object of figure 1
#' @export
make_fig1 <- function(home_ranges, land, islands, bathy) {
  fig1_extent <- extent(home_ranges) * 1.2
  ggplot() +
    geom_sf(data = land, fill = "#989898", color = NA) +
    geom_sf(data = islands, fill = "#989898", color = NA) +
    geom_sf(aes(color = depth), data = bathy) +
    geom_sf(aes(fill = ud_level), data = home_ranges, color = NA, alpha = 0.8) +
    scale_x_continuous(breaks = c(-5, 0, 5)) +
    scale_y_continuous(breaks = seq(52, 60, by = 2)) +
    scale_fill_manual(values = c(`25%` = "#DB602B",
                                 `50%` = "#E9C14C",
                                 `75%` = "#FFFF5A",
                                 `95%` = "#D5D5D5")) +
    scale_color_manual(values = c(`-50 m` = "#DBDBDB",
                                  `-150 m` = "#C4C4C4",
                                  `-250 m` = "#787878")) +
    labs(fill = "UD") +
    guides(fill = guide_legend(reverse = TRUE)) +
    coord_sf(xlim = fig1_extent[1:2],
             ylim = fig1_extent[3:4]) +
    facet_grid(cols = vars(age)) +
    theme_minimal() +
    theme(strip.background = element_blank(),
          strip.text = element_blank())

  ggsave("analysis/figures/fig1.pdf",
         height = 80, width = 160,
         units = "mm", dpi = 600)

  "analysis/figures/fig1.pdf"
}
