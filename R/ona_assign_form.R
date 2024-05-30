#' Assign / Re-assign a form to a project
#'
#' @param project_id Project ID
#' @param form_id Form ID
#'
#' @return [httr2::response]
#' @export
ona_assign_form <- function(project_id, form_id) {
  req <- ona_get_authenticated_request() |>
    httr2::req_url_path_append("projects") |>
    httr2::req_url_path_append(project_id) |>
    httr2::req_url_path_append("forms") |>
    httr2::req_body_form(formid = form_id) |>
    httr2::req_method("POST")
  httr2::req_perform(req)
}
