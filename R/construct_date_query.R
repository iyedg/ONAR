construct_date_query <- function(
    start_date = NULL,
    end_date = NULL) {
  json_obj <- list()
  if (!is.null(start_date) && !is.null(end_date)) {
    start_date_iso8601 <- lubridate::format_ISO8601(start_date + lubridate::seconds(1) - lubridate::seconds(1))
    end_date_iso8601 <- lubridate::format_ISO8601(end_date + lubridate::days(1) - lubridate::seconds(1))

    json_obj <- list(
      `_submission_time` = list(
        `$gte` = start_date_iso8601,
        `$lte` = end_date_iso8601
      )
    )
  } else if (!is.null(start_date)) {
    start_date_iso8601 <- lubridate::format_ISO8601(start_date + lubridate::seconds(1) - lubridate::seconds(1))

    json_obj <- list(
      `_submission_time` = list(
        `$gte` = start_date_iso8601
      )
    )
  } else if (!is.null(end_date)) {
    end_date_iso8601 <- lubridate::format_ISO8601(end_date + lubridate::days(1) - lubridate::seconds(1))

    json_obj <- list(
      `_submission_time` = list(
        `$lte` = end_date_iso8601
      )
    )
  }

  json_str <- jsonlite::toJSON(json_obj, auto_unbox = TRUE)
  return(json_str)
}
