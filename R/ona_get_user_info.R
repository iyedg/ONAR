ona_get_user_info <- function(username) {
  req <- ona_get_authenticated_request() |>
    httr2::req_url_path_append("users") |>
    httr2::req_url_path_append(username)

  resp <- req |>
    httr2::req_perform() |>
    httr2::resp_body_json()

  return(resp)
}
