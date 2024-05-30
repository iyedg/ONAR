#' Get the list of projects available to the authenticated user
#'
#' @return An HTTP response
#' @seealso [httr2::response()]
#' @export
ona_projects <- function() {
  response <- ona_get_authenticated_request() |>
    httr2::req_url_path_append("projects") |>
    httr2::req_perform()

  response |>
    httr2::resp_body_json()
}
