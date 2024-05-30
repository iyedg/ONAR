#' Check the Status of form exports on ONA
#'
#' @param form_id the unique identifier of the form
#' @param job_uuid the UUID of the export job, retrieved via [create_ona_form_export()]
#'
#' @return A HTTP Response
#' @noRd
check_ona_form_export_status <- function(
    form_id,
    job_uuid) {
  response <- ona_get_authenticated_request() |>
    httr2::req_url_path_append("forms") |>
    httr2::req_url_path_append(form_id) |>
    httr2::req_url_path_append("export_async.json") |>
    httr2::req_url_query(job_uuid = job_uuid)

  response |>
    httr2::req_perform()
}
