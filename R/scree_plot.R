#' Scree plot
#'
#' A scree plot made in ggplot2
#'
#' @param data A data frame
#' @param w The name of a weighting variable. Defaults to 1 if not given.
#' @param ... variables to run PCA on. tidyselect methods can be used.
#'
#' @import rlang
#' @import dplyr
#' @import ggplot2
#' @importFrom FactoMineR PCA
#' @importFrom factoextra get_eigenvalue
#'
#' @return A scree plot in ggplot2
#' @export
#'
#' @examples
#'
#' scree_plot(iris, -Species)

scree_plot <- function(data, w = NULL, ...) {
  vars <- enquos(...)
  w <- enquo(w)

  if (!quo_is_null(w)) {
    weights <- pull(data, {{w}})
  }

  data |>
    select(!!! vars) |>
    mutate(across(everything(), ~ c(scale(.x)))) |>
    PCA(row.w = weights) |>
    get_eigenvalue() |>
    as_tibble() |>
    mutate(dims = row_number()) |>
    ggplot(aes(x = dims, y = eigenvalue)) +
    geom_line() +
    geom_point() +
    geom_hline(yintercept = 1)
}
