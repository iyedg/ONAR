#' Get the XLSForm Representation
#'
#' @param form_id Form Unique Identifier
#'
#' @return A list of data frames, where the names of the list are sheet names
#' @export
ona_xlsform <- function(
    form_id) {
  response <- ona_get_authenticated_request() |>
    httr2::req_url_path_append("forms") |>
    httr2::req_url_path_append(form_id) |>
    httr2::req_url_path_append("form.xlsx")
  path <- tempfile(fileext = ".xlsx")
  response |>
    httr2::req_perform(path = path)

  form <- read_xlsform(path)
  return(form)
}
