#' Create Banner Tables
#'
#' @description
#' Creates a banner table un xlsx format to then analyze outside of R how the segments differ across the variables selected
#'
#' @param data A data frame with the segment variable and the variables to be analyzed
#' @param seg The name of the segment variable, unquoted
#' @param labels A vector of the labels for the variables being analyzed
#' @param ... Variables to analyze
#'
#' @import dplyr
#' @import openxlsx
#' @import rlang
#' @importFrom purrr map
#' @importFrom purrr map_int
#' @importFrom purrr walk2
#' @importFrom tidyr pivot_wider
#'
#' @return An excel file with banner tables results
#' @export
#'
#' @examples
#' \dontrun{
#' create_cluster_tables(iris, Species, labels = c("Petal Length", "Sepal Length"),
#'   Petal.Length, Sepal.Length)
#' }

create_cluster_tables <- function(data, seg, labels, ...) {
  vars <- enquos(...)

  tables <- map(vars, function(x) {
    data |>
      count({{ seg }}, !! x) |>
      mutate(perc = n / sum(n), .by = {{ seg }}) |>
      select(-n) |>
      mutate({{ seg }} := paste("Segment", {{ seg }})) |>
      pivot_wider(names_from = {{ seg }}, values_from = perc)
  })

  start_rows <- cumsum(c(2, map_int(tables, function(x) nrow(x) + 3)))
  last_row <- start_rows[length(start_rows)]
  start_rows <- start_rows[-length(start_rows)]

  rr_numbers <- createStyle(halign = "center", numFmt = "0%")

  wb <- createWorkbook()

  addWorksheet(wb, sheetName = "table")
  walk2(tables, start_rows, ~ writeData(wb, "table", x = .x, startRow = .y, startCol = 1))
  walk2(labels, start_rows, ~ writeData(wb, "table", x = .x, startRow = .y - 1, startCol = 1))

  addStyle(wb, "table", rr_numbers, rows = 1:last_row, cols = 2:(ncol(tables[[1]])), gridExpand = TRUE)

  saveWorkbook(wb, paste0(deparse(substitute(seg)), "_results.xlsx"), overwrite = TRUE)
}
