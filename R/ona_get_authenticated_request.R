#' Get an ONA API request with HTTP Basic Authentication
#'
#' @param base_url ONA Base API URL
#' @param ona_username ONA Username, defaults to the environment variable ONA_USERNAME
#' @param ona_password ONA Password, defaults to the environment variable ONA_PASSWORD
#'
#' @return A request object with HTTP Basic Authentication
#' @seealso [httr2::request]
#' @seealso [httr2::req_auth_basic]
#' @export
ona_get_authenticated_request <- function(
    base_url = "https://api.ona.io/api/v1",
    ona_username = Sys.getenv("ONA_USERNAME", unset = NA),
    ona_password = Sys.getenv("ONA_PASSWORD", unset = NA)) {
  httr2::request(base_url) |>
    httr2::req_auth_basic(
      username = ona_username,
      password = ona_password
    )
}
