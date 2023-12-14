#' Analyzing across cluster solutions
#'
#' @description
#' `cluster_split_matrix` returns a matrix breaking down the split betwween 2 given solutions.
#'
#' @param data data frame with cluster solutions
#' @param ... segment variable names, unquoted. tidyselect methods can be used.
#'
#' @import rlang
#' @import dplyr
#' @import tidyr
#'
#' @return A table showing the split between 2 solutions
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
#' cluster_split_matrix(segs_df, sol_2, sol_3)

cluster_split_matrix <- function(data, ...) {
  vars <- enquos(...)

  stopifnot("Please provide exactly 2 cluster variables to split into a matrix" = length(vars) == 2)

  data |>
    select(!!! vars) |>
    mutate(rn = row_number()) |>
    pivot_longer(cols = -rn) |>
    mutate(value = paste(name, value, sep = "_")) |>
    pivot_wider(names_from = name, values_from = value) |>
    select(-rn) |>
    count(!!! vars) |>
    pivot_wider(names_from = !! vars[[2]], values_from = n, values_fill = 0) |>
    rename(` ` = !! vars[[1]])
}
