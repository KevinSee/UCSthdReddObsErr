#' @title Estimate Correlation Between Reaches
#'
#' @description Estimates correlation between reaches, to be used when calculating standard error across multiple reaches so reaches aren't necessarily treated independently
#'
#' @author Kevin See
#'
#' @inheritParams estimateRedds
#' @param date_nm quoted name of column in {redd_df} listing date of each survey
#' @param redd_nm quoted name of column in {redd_df} listing number of redds found during that survey
#' @param reach_nm quoted name of column in {redd_df} listing name of reach
#'
#' @import dplyr corrr lubridate
#' @return dataframe
#' @export

correlateRchs = function(redd_df = NULL,
                         date_nm = "SurveyDate",
                         redd_nm = "NewRedds",
                         reach_nm = "Reach",
                         use = "pairwise.complete.obs",
                         ...) {
  if(is.null(redd_df)) {
    stop("redd data must be supplied")
  }

  # check the class of survey date column
  date_class <- redd_df |>
    pull({{ date_nm}}) |>
    class()
  if(date_class == "Date") {
    redd_df <- redd_df |>
      mutate(across({{ date_nm }},
                    as.POSIXct))
  }

  cor_mat <- redd_df |>
    rename(redds = {{ redd_nm }},
           reach = {{ reach_nm }},
           surv_date = {{ date_nm }}) |>
    mutate(surv_period = lubridate::week(surv_date)) |>
    group_by(reach, surv_period) %>%
    summarize(across(redds,
                     mean,
                     na.rm = T)) %>%
    pivot_wider(names_from = reach,
                values_from = redds,
                names_sort = T) |>
    select(-surv_period) |>
    # corrr::correlate(...)
    stats::cor(use = use,
               ...)

  return(cor_mat)

}
