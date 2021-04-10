#' Make figure 2
#'
#' Figure 2 shows the observed home range overlap of adult and immature Northern
#' Gannets compared the null distribution
#'
#' @param obs_ba Observed Bhattacharyya's affinity of adult and immature home
#'   ranges (see `observed_ba`)
#' @param null_ba Null distribution of Bhattacharyya's affinity of adult and
#'   immature home ranges (see `sim_null_ba`)
#'
#' @return File path to figure 2
#' @export
make_fig2 <- function(obs_ba, null_ba) {
  ggplot() +
    geom_density(aes(ba, ..scaled.., fill = percent), null_ba, color = NA) +
    geom_vline(aes(xintercept = ba), obs_ba, linetype = 2) +
    geom_text(aes(ba, y = 1, label = percent), obs_ba,
              hjust = 1, vjust = 1, size = 2.5) +
    scale_fill_manual("UD",
                      values = c(`50%` = "#E69326",
                                 `95%` = "#C8C8C8")) +
    labs(x = "Bhattacharyya’s affinity",
         y = "frequency density") +
    xlim(0, 1) +
    theme_classic() +
    theme(axis.text = element_text(size = 8),
          axis.title = element_text(size = 8),
          legend.justification = c(0, 1),
          legend.position = c(0.75, 1),
          legend.text = element_text(size = 8),
          legend.title = element_text(size = 8))

  ggsave("analysis/figures/fig2.pdf",
         height = 40, width = 80,
         units = "mm", dpi = 600)

  "analysis/figures/fig2.pdf"
}
