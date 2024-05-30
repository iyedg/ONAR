#' Create an export on ONA asynchronously
#'
#' @param form_id The form's unique identifier
#' @param format one of `csv`, `xls`, `savzip`, `csvzip`, `kml`, `osm`, and `gsheets`
#' @param do_not_split_select_multiples Split select multiple answers into separate columns when set to `FALSE`
#' @param include_images Include images when set to `TRUE`
#' @param remove_group_name Remove prefixed group names when set to `TRUE`
#' @param include_labels Include column labels when set to `TRUE`
#' @param include_labels_only Exclude question names from the header when set to `TRUE`
#' @param binary_select_multiples Use 0 or 1 in split select multiples as opposed to TRUE and FALSE when set to `TRUE`
#' @param value_select_multiples Use values for select multiple questions
#' @param show_choice_labels Use choice labels
#' @param include_reviews Include reviews when set to `TRUE`
#' @param language The export language, defaults to English
#' @param group_delimiter The character used to separate group names from column names. Defaults to '/'
#' @param start_date Filter for submissions on `start_date` or later
#' @param end_date Filter for submissions on `start_date` or earlier
#'
#' @return A HTTP response. In case of success it will contain a job id for the export
#' @seealso [httr2::response()]
ona_create_form_export <- function(
    form_id,
    format = "xlsx",
    do_not_split_select_multiples = FALSE,
    include_images = FALSE,
    remove_group_name = TRUE,
    include_labels = FALSE,
    include_labels_only = FALSE,
    binary_select_multiples = FALSE,
    value_select_multiples = TRUE,
    show_choice_labels = TRUE,
    include_reviews = FALSE,
    language = "English (en)",
    group_delimiter = "/",
    start_date = NULL,
    end_date = NULL) {
  req <- ona_get_authenticated_request() |>
    httr2::req_url_path_append("forms") |>
    httr2::req_url_path_append(form_id) |>
    httr2::req_url_path_append("export_async.json") |>
    httr2::req_url_query(
      format = format,
      do_not_split_select_multiples = do_not_split_select_multiples,
      include_images = include_images,
      remove_group_name = remove_group_name,
      include_labels = include_labels,
      include_labels_only = include_labels_only,
      binary_select_multiples = binary_select_multiples,
      value_select_multiples = value_select_multiples,
      show_choice_labels = show_choice_labels,
      include_reviews = include_reviews,
      language = language,
      group_delimiter = group_delimiter
    )

  if (!is.null(start_date) || !is.null(end_date)) {
    req <- req |>
      httr2::req_url_query(
        query = construct_date_query(
          start_date = start_date,
          end_date = end_date
        )
      )
  }


  req |>
    httr2::req_perform()
}
