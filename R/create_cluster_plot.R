#' Create Cluster Scatter plot
#'
#' @param data The data frame including only the variables used for the segmentation, as well as the segment variable.
#' @param seg The name of the segmentation variable in the data frame, unquoted.
#' @param ThreeD Logical indicating whether the plot should be 3D or 2D.
#'
#' @import dplyr
#' @import ggplot2
#' @importFrom plotly plot_ly
#' @importFrom plotly add_markers
#' @importFrom plotly add_text
#' @importFrom psych principal
#' @importFrom tibble add_column
#'
#' @return A scatterplot showing the location of the segments across the data space.
#' @export
#'
#' @examples
#' create_cluster_plot(iris, Species)
#' create_cluster_plot(iris, Species, ThreeD = TRUE)

create_cluster_plot <- function(data, seg, ThreeD = FALSE) {
  if (ThreeD) {
    nfactors <- 3
  } else {
    nfactors <- 2
  }

  pca <- data |>
    select(-{{ seg }}) |>
    mutate(across(everything(), ~ c(scale(.x)))) |>
    principal(nfactors = nfactors)

  scores <- pca[["scores"]] |>
    unclass() |>
    as_tibble() |>
    add_column(seg = pull(data, {{ seg }}))

  centroids <- summarize(scores, across(everything(), mean), .by = seg)

  if (ThreeD) {
    plot <- plot_ly() |>
      add_markers(x = scores[["RC1"]], y = scores[["RC2"]], z = scores[["RC3"]],
                  color = scores[["seg"]],
                  showlegend = FALSE) |>
      add_text(x = centroids[["RC1"]], y = centroids[["RC2"]], z = centroids[["RC3"]], text = centroids[["seg"]])
  } else {
    plot <- ggplot() +
      geom_point(data = scores, aes(x = RC1, y = RC2, colour = seg), show.legend = FALSE) +
      geom_label(data = centroids, aes(x = RC1, y = RC2, label = seg, colour = seg), show.legend = FALSE) +
      geom_hline(yintercept = mean(scores[["RC2"]])) +
      geom_vline(xintercept = mean(scores[["RC1"]])) +
      theme(axis.ticks = element_blank(),
            panel.grid = element_blank(),
            panel.background = element_blank())
  }

  plot
}
