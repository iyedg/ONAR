#' Get Project Info from ONA
#'
#' @param project_id Project Identifier on ONA
#'
#' @return JSON response from ONA
#' @export
ona_get_project_info <- function(project_id) {
  req <- ona_get_authenticated_request() |>
    httr2::req_url_path_append("projects") |>
    httr2::req_url_path_append(project_id)
  resp <- httr2::req_perform(req)
  httr2::resp_body_json(resp)
}

#' Get User List working on an ONA project
#'
#' @param project_id Project Identifier on ONA
#'
#' @return A list of ONA usernames
#' @export
ona_get_project_users <- function(project_id) {
  project_info <- ona_get_project_info(project_id)

  project_info$users |>
    purrr::map(.f = \(x) purrr::pluck(x, "user")) |>
    purrr::list_c()
}
