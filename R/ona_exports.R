#' A list of ONA exports
#'
#' @param form_id A Unique Identifier for a form
#'
#' @return data.frame
#' @export
ona_exports <- function(
    form_id) {
  response <- ona_get_authenticated_request() |>
    httr2::req_url_path_append("export.json") |>
    httr2::req_url_query(xform = form_id)

  response |>
    httr2::req_perform() |>
    httr2::resp_body_json() |>
    tidyjson::spread_all()
}
