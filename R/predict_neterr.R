#' @title Predict Net Error
#'
#' @description Predict the net error of a steelhead redd survey, based on published model
#'
#' @author Kevin See
#'
#' @param data dataframe containing columns with covariates in observer error model
#'
#' @import dplyr
#' @return tibble
#' @export

predict_neterr = function(data) {

  if(sum(!attr(net_err_mod$terms, "term.labels") %in% names(data)) > 0) {
    miss_covar = attr(net_err_mod$terms, "term.labels")[!attr(net_err_mod$terms, "term.labels") %in% names(data)]
    stop(paste("Missing covariates in dataset:", paste(miss_covar, collapse = ', ')))
  }

  pred_df = data %>%
    gather(metric, value, one_of(covar_center$metric)) %>%
    left_join(covar_center) %>%
    mutate(value = (value - mu) / stddev ) %>%
    select(-mu, -stddev) %>%
    spread(metric, value) %>%
    bind_cols(predict(net_err_mod,
                      newdata = .,
                      backtransform = T,
                      type = 'link',
                      se.fit = T) %>%
                tbl_df() %>%
                select(NetError = fit,
                       NetErrorSE = se.fit))

  pred_df = pred_df %>%
    select(-one_of(covar_center$metric)) %>%
    left_join(data) %>%
    select(one_of(names(data)), everything())

  return(pred_df)

}
