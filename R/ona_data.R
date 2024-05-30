#' Download Form Data
#'
#' A thin wrapper around [ona_create_form_export()] that handles checking
#' the export status and saves it in `path` when the export succeeds.
#'
#' @param form_id The form's unique identifier
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
#' @param ... Arguments passed to [readxl::read_excel()]
#'
#' @return Data Frame
#' @export
ona_data <- function(
    form_id,
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
    end_date = NULL,
    ...) {
  cli::cli_alert_info("Creating ONA Form Export")
  resp_export <- ona_create_form_export(
    form_id,
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
    group_delimiter = group_delimiter,
    start_date = start_date,
    end_date = end_date
  ) |>
    httr2::resp_body_json()
  export_job_uuid <- resp_export$job_uuid

  if (is.null(export_job_uuid)) {
    export_url <- resp_export$export_url
  } else {
    # TODO: add counter to limit retries
    repeat {
      cli::cli_alert_info("Checking the status of the export for {form_id}")

      resp_status <- check_ona_form_export_status(
        form_id = form_id,
        job_uuid = export_job_uuid
      ) |> httr2::resp_body_json()

      if (
        resp_status$job_status == "SUCCESS"
      ) {
        export_url <- resp_status$export_url
        break
      }
    }
  }
  # TODO: handle when the format is not an excel file
  cli::cli_alert_info("Downloading data")
  path <- tempfile(fileext = ".xlsx") # TODO: handle format argument
  resp <- ona_get_authenticated_request() |>
    httr2::req_url(export_url) |>
    httr2::req_perform(path = path)

  sheets <- readxl::excel_sheets(path) |>
    janitor::make_clean_names()

  if (length(sheets) > 1) {
    cli::cli_alert_warning("Multiple sheets found. This may mean that you have repeat groups in your form.")
    cli::cli_li(sheets)

    res <- sheets |>
      purrr::map(
        .f = ~ readxl::read_excel(path = path, sheet = .x, ...)
      ) |>
      rlang::set_names(sheets)

    return(res)
  }

  return(readxl::read_excel(path = path, ...))
}
