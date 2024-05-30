#' Register a new user on ONA
#'
#' @param user_name User Name
#' @param email Email
#' @param name Display Name
#' @param password Password
#'
#' @return a response from [httr2::response()]
#' @export
register_user <- function(
    user_name,
    email,
    name,
    password) {
  base_url <- "https://api.ona.io/api/v1"
  request <- httr2::request(base_url) |>
    httr2::req_url_path_append("profiles") |>
    httr2::req_body_json(list(
      username = user_name,
      email = email,
      name = name,
      require_auth = F,
      password = password
    )) |>
    httr2::req_method("POST") |>
    httr2::req_error(body = function(resp) {
      httr2::resp_body_string(resp)
    })

  response <- request |> httr2::req_perform()
  return(response)
}
