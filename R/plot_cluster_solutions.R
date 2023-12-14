#' Analyzing across cluster solutions
#'
#' @description
#' `plot_cluster_solutions` returns an alluvial plot displaying how segments are broken out from one cluster solution to another.
#'
#' @param data data frame with cluster solutions
#' @param ... segment variable names, unquoted.
#'
#' @import rlang
#' @import dplyr
#' @import ggplot2
#' @import ggforce
#'
#' @return An alluvial plot made using ggplot2
#' @export
#'
#' @examples
#'
#' set.seed(1)
#' segs_df <- data.frame(
#'   sol_2 = round(runif(100, min = 1, max = 2), 0),
#'   sol_3 = round(runif(100, min = 1, max = 3), 0),
#'   sol_4 = round(runif(100, min = 1, max = 4), 0)
#' )
#'
#' plot_cluster_solutions(segs_df, sol_2, sol_3, sol_4)

plot_cluster_solutions <- function(data, ...) {
  vars <- enquos(...)

  data |>
    count(!!! vars) |>
    mutate(across(c(!!! vars), as.character)) |>
    gather_set_data(seq_along(vars)) |>
    ggplot(aes(x, id = id, split = y, value = n)) +
    geom_parallel_sets(aes(fill = !! vars[[length(vars)]]), alpha = 0.3, axis.width = 0.1) +
    geom_parallel_sets_axes(axis.width = 0.1) +
    geom_parallel_sets_labels(colour = 'white') +
    theme_void() +
    theme(legend.position = "none")
}
