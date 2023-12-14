#' Finalized Principal Components Table
#'
#' Runs a PCA with a pre-specified number of factors and returns a table with
#' the variables ordered by their factors.
#'
#' @param data A data frame with only the variables to run the PCA on.
#' @param nfactors The number of factors desired.
#' @param labels_df A data frame that lists the variable names in a "name"
#'   variable and the labels of the those variable in a "label" variable.
#'
#' @import dplyr
#' @importFrom psych principal
#' @importFrom tidyr pivot_longer
#'
#' @return A data frame with the variables ordered by their corresponding factors
#' @export
#'
#' @examples
#' data <- as.data.frame(Harman74.cor$cov)
#'
#' labels_df <- data.frame(
#'   name = names(data),
#'   label = gsub("([[:lower:]][[:lower:]])([[:upper:]])", "\\1 \\2", names(data)))
#'
#' suppressWarnings(
#'   create_final_pca(data = data, nfactors = 4, labels_df = labels_df)
#'   )

create_final_pca <- function(data, nfactors, labels_df) {
  pca <- data |>
    mutate(across(everything(), ~ c(scale(.x)))) |>
    principal(nfactors = nfactors)

  loadings <- pca$loadings |>
    unclass() |>
    as_tibble() |>
    mutate(name = names(data))

  max_loadings <- loadings |>
    pivot_longer(-name, names_to = "highest_RC") |>
    group_by(name) |>
    filter(abs(value) == max(abs(value))) |>
    ungroup() |>
    select(name, highest_RC)

  order_loadings <- loadings |>
    left_join(max_loadings, by = join_by(name)) |>
    pivot_longer(-c(name, highest_RC), names_to = "key") |>
    filter(key == highest_RC) |>
    group_by(highest_RC) |>
    arrange(desc(abs(value))) |>
    mutate(order = row_number()) |>
    ungroup() |>
    select(name, highest_RC, order)

  loadings |>
    left_join(labels_df, by = join_by(name)) |>
    left_join(max_loadings, by = join_by(name)) |>
    left_join(order_loadings, by = join_by(name, highest_RC)) |>
    arrange(highest_RC, order) |>
    relocate(name, label, everything()) |>
    select(-order)
}
