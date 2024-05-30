ona_get_value_labels <- function(
    var_name,
    form) {
  # TODO: Handle missing var_name
  question_list_name <- form$survey |>
    dplyr::filter(
      .data[["name"]] == var_name
    ) |>
    dplyr::pull("list_name")
  form$choices |> dplyr::filter(.data[["list_name"]] == question_list_name)
}
