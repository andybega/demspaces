
#' ds_fcast constructor
#'
#' @param x a data frame of forecasts
#'
#' @export
#'
#' @examples
#' data("states")
#'
#' mdl <- ds_logistic_reg("v2x_veracc_osp", states)
#' preds <- predict(mdl, new_data = states)
#' head(ds_fcast(preds))
ds_fcast <- function(x) {
  if (inherits(x, "ds_fcast")) return(x)
  xnames <- names(x)
  stopifnot(setequal(xnames, c("outcome", "from_year", "for_years", "gwcode",
                               "p_up", "p_same", "p_down")))
  class(x) <- c("ds_fcast", class(x))
  x
}

#' Score forecasts
#'
#' @param x [ds_fcast()]
#' @param truth_data states data
#'
#' @examples
#' data("states")
#'
#' mdl <- ds_logistic_reg("v2x_veracc_osp", states)
#' preds <- predict(mdl, new_data = states)
#' fcasts <- ds_fcast(preds)
#' score_ds_fcast(fcasts, states)
#' @export
score_ds_fcast <- function(x, truth_data) {
  x <- ds_fcast(x)
  x <- add_truth_data_ds_fcast(x, truth_data)

  suppressWarnings({
  stats <- list(
    "ROC-AUC up"   = x %>% yardstick::roc_auc(up, p_up) %>% `[[`(".estimate"),
    "ROC-AUC down" = x %>% yardstick::roc_auc(down, p_down) %>% `[[`(".estimate"),
    "PR-AUC up"    = x %>% yardstick::pr_auc(up, p_up) %>% `[[`(".estimate"),
    "PR-AUC down"  = x %>% yardstick::pr_auc(down, p_down) %>% `[[`(".estimate")
  ) %>% tibble::enframe(value = "Value") %>%
    tidyr::unnest(Value) %>%
    tidyr::separate(name, into = c("Measure", "Direction"), sep = " ")
  })
  stats
}


#' Add truth data to forecasts
#'
#' @param x [ds_fcast()]
#' @param truth_data states data
#'
#' @export
add_truth_data_ds_fcast <- function(x, truth_data) {
  stopifnot(inherits(x, "ds_fcast"))

  truth_data <- truth_data %>%
    dplyr::select(gwcode, year, dplyr::ends_with("next2"))

  truth_data <- truth_data %>%
    tidyr::gather(var, value, -gwcode, -year) %>%
    dplyr::mutate(change = dplyr::case_when(
      stringr::str_detect(var, "\\_up\\_") ~ "up",
      stringr::str_detect(var, "\\_down\\_") ~ "down",
      TRUE ~ NA_character_
    )) %>%
    dplyr::mutate(var = stringr::str_replace(var, "dv\\_", ""),
                  var = stringr::str_replace(var, "\\_(up|down)\\_next2", ""),
                  # yardstick requires factor labels
                  value = factor(value, levels = c("1", "0")))

  truth_data <- truth_data %>%
    tidyr::spread(change, value)

  xaug <- x %>%
    dplyr::left_join(truth_data, by = c("gwcode", "from_year" = "year", "outcome" = "var"))
  xaug
}
