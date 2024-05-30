#' Read an XLSForm Excel file
#'
#' @param path path to the XLSForm file
#' @param ... Added for extensibility
#'
#' @return list of data frames where each sheet corresponds to a data frame
#' @export
read_xlsform <- function(
    path, ...) {
  sheets <- path |>
    readxl::excel_sheets()

  form <- purrr::map(sheets, readxl::read_excel, path = path) |>
    rlang::set_names(sheets)

  form$survey <- form$survey |>
    janitor::clean_names() |>
    janitor::remove_empty(which = c("cols", "rows")) |>
    dplyr::mutate(type = dplyr::if_else(
      stringr::str_detect(.data[["type"]], "(begin|end)\\s+group"),
      stringr::str_trim(.data[["type"]]) |> stringr::str_replace(" ", "_"),
      .data[["type"]]
    )) |>
    tidyr::separate_wider_delim(
      cols = "type",
      delim = " ",
      names = c("type", "list_name"),
      too_few = "align_start",
      too_many = "drop"
    )

  form$choices <- form$choices |>
    janitor::clean_names() |>
    janitor::remove_empty(which = c("rows", "cols"))

  if ("external_choices" %in% names(form)) {
    form$external_choices <- form$external_choices |>
      janitor::clean_names() |>
      janitor::remove_empty(which = c("rows", "cols"))
  }

  return(form)
}
