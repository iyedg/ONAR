#' Share an ONA Project with a user and specify their role
#'
#' @param project_id Unique Identifier of the Project
#' @param username The username with whom the project will be shared
#' @param role One of `readonly`, `dataentry`, `editor`, and `manager`
#'
#' @return A HTTP response. On success the response will be empty with a HTTP Status code 204
#' @export
ona_share_project <- function(
    project_id,
    username,
    role = "dataentry") {
  response <- ona_get_authenticated_request() |>
    httr2::req_url_path_append("projects") |>
    httr2::req_url_path_append(project_id) |>
    httr2::req_url_path_append("share") |>
    httr2::req_body_form(
      username = username,
      role = role
    ) |>
    httr2::req_method("PUT")
  response |>
    httr2::req_perform()
}
