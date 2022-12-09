#' @title Estimate Redds
#'
#' @description Estimates true number of redds across a series of reaches, using a Gaussian area-under-the-curve model where possible and incorporating estimates of observer error if possible.
#'
#' @author Kevin See
#'
#' @param redd_df dataframe containing redd data
#' @param group_var vector of column names from {redd_df} to group results by
#' @param new_redd_nm quoted name of column in {redd_df} listing number of new redds found during that survey
#' @param vis_redd_nm quoted name of column in {redd_df} listing number of visible redds present during that survey
#' @param net_err_nm quoted name of column in {redd_df} listing estimate of net error for that survey
#' @param net_se_nm quoted name of column in {redd_df} listing standard error of net error estimate for that survey
#' @param min_non0_wks minimum number of weeks with at least one new redd observed
#' @param min_redds minimum number of total redds observed
#'
#' @import dplyr msm
#' @return dataframe
#' @export

estimateRedds = function(redd_df = NULL,
                         group_vars = c(River, Reach, Index, SurveyType),
                         new_redd_nm = "NewRedds",
                         vis_redd_nm = "VisibleRedds",
                         net_err_nm = "NetError",
                         net_se_nm = "NetErrorSE",
                         min_non0_wks = 3,
                         min_redds = 2) {

  if(is.null(redd_df)) {
    stop("redd data must be supplied")
  }

  redd_results = redd_df %>%
    rename(NewRedds = {{new_redd_nm}},
           VisibleRedds = {{vis_redd_nm}},
           NetError = {{net_err_nm}},
           NetErrorSE = {{net_se_nm}}) %>%
    group_by(across({{ group_vars }})) %>%
    summarise(n_weeks = n(),
              n_non0_wks = sum(NewRedds > 0, na.rm=T),
              tot_feat = sum(NewRedds, na.rm=T),
              err_est = mean(NetError[VisibleRedds > 0]),
              err_se = mean(NetErrorSE[VisibleRedds > 0]),
              .groups = "drop") %>%
    mutate(err_se = ifelse(is.na(err_est), 0, err_se),
           err_est = ifelse(is.na(err_est), 1, err_est)) %>%
    full_join(redd_df %>%
                group_by(across({{ group_vars }})) %>%
                nest()) %>%
    mutate(gauc_list = map(data,
                           .f = function(x) {
                             mod_df = x %>%
                               select(redds = NewRedds) %>%
                               mutate(day = 1:n())

                             v_vec = x %>%
                               filter(VisibleRedds > 0) %>%
                               summarise_at(vars(NetError, NetErrorSE),
                                            list(mean),
                                            na.rm = T) %>%
                               mutate(NetErrorSE = if_else(is.na(NetError),
                                                           0,
                                                           NetErrorSE),
                                      NetError = if_else(is.na(NetError),
                                                         1,
                                                         NetError))

                             # # if you want to assume no observer error
                             # v_vec = tibble(NetError = 1,
                             #                NetErrorSE = 0)

                             res_list = fit_gauc(data = mod_df,
                                                 v = v_vec$NetError,
                                                 v_se = v_vec$NetErrorSE)
                             return(res_list)
                           })) %>%
    mutate(converged = map_lgl(gauc_list,
                               .f = function(x) {
                                 x$model$converged
                               })) %>%
    mutate(redd_est = map_dbl(gauc_list,
                              .f = 'E'),
           redd_se = map_dbl(gauc_list,
                             .f = 'E_se')) %>%
    mutate(GAUC = if_else(!converged | n_non0_wks < min_non0_wks | tot_feat < min_redds,
                          F, T)) %>%
    rowwise() %>%
    mutate(redd_est = if_else(!GAUC,
                              tot_feat / err_est,
                              redd_est),
           redd_se = if_else(!GAUC,
                             msm::deltamethod(~ x1 / x2,
                                              mean = c(tot_feat, err_est),
                                              cov = diag(c(0, err_se)^2)),
                             redd_se)) %>%
    ungroup() %>%
    mutate_at(vars(redd_est),
              list(round_half_up))

  return(redd_results)
}

