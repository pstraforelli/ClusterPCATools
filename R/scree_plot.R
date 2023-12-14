#' Scree plot
#'
#' A scree plot made in ggplot2
#'
#' @param data A data frame
#' @param ... variables to run PCA on. tidyselect methods can be used.
#'
#' @import rlang
#' @import dplyr
#' @import ggplot2
#' @importFrom stats prcomp
#' @importFrom factoextra get_eigenvalue
#'
#' @return A scree plot in ggplot2
#' @export
#'
#' @examples
#'
#' scree_plot(iris, -Species)

scree_plot <- function(data, ...) {
  vars <- enquos(...)

  data |>
    select(!!! vars) |>
    mutate(across(everything(), ~ c(scale(.x)))) |>
    prcomp() |>
    get_eigenvalue() |>
    as_tibble() |>
    mutate(dims = row_number()) |>
    ggplot(aes(x = dims, y = eigenvalue)) +
    geom_line() +
    geom_point() +
    geom_hline(yintercept = 1)
}
