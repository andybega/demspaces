
#' Logistic regression
#'
#' Demonstrates the interface. Otherwise only difference is that it internally
#' normalizes input data before fitting and predicting.
#'
#' @param space (`character(1)`) \cr
#'   Name of the V-Dem indicator for a democratic space
#' @param data ([base::data.frame()])
#'   A data frame or similar object
#'
#' @examples
#' data("states")
#'
#' mdl <- ds_logistic_reg("v2x_veracc_osp", states)
#' preds <- predict(mdl, new_data = states)
#'
#' @export
ds_logistic_reg <- function(space, data) {

  df <- data
  yname <- space

  # drop DV vars that we should exclude, except for our actual outcome pair
  ynameup   <- paste0("dv_", yname, "_up_next2")
  ynamedown <- paste0("dv_", yname, "_down_next2")

  df <- df %>%
    dplyr::select(-dplyr::starts_with("dv"), dplyr::one_of(c(ynameup, ynamedown)))

  # split the data; last year is reserved for forecast, use complete data
  # before that for training
  train_data <- df %>%
    dplyr::filter(!is.na(df[[ynameup]]),
                  !is.na(df[[ynamedown]])) %>%
    dplyr::select(-.data$gwcode, -.data$year)

  train_data <- train_data %>%
    dplyr::filter(stats::complete.cases(.))

  updata <- train_data %>%
    dplyr::select(-dplyr::one_of(ynamedown))
  up_mdl <- logistic_reg(x = updata %>% dplyr::select(-dplyr::one_of(ynameup)),
                         y = updata[, ynameup])

  downdata <- train_data %>%
    dplyr::select(-dplyr::one_of(ynameup))
  down_mdl <- logistic_reg(x = downdata %>% dplyr::select(-dplyr::one_of(ynamedown)),
                           y = downdata[, ynamedown])

  structure(
    list(
      up_mdl   = up_mdl,
      down_mdl = down_mdl),
    yname = space,
    class = "ds_logistic_reg"
  )
}

#' @export
#' @importFrom stats predict
predict.ds_logistic_reg <- function(object, new_data, ...) {

  up_mdl   <- object$up_mdl
  down_mdl <- object$down_mdl
  yname    <- attr(object, "yname")

  p_up     <- predict(up_mdl,   new_data = new_data)[["p_1"]]
  p_down   <- predict(down_mdl, new_data = new_data)[["p_1"]]
  p_same   <- 1 - p_up - p_down

  fcast <- data.frame(
    outcome   = yname,
    from_year = new_data$year,
    for_years = paste0(new_data$year + 1, " - ", new_data$year + 2),
    gwcode = new_data$gwcode,
    p_up   = p_up,
    p_same = p_same,
    p_down = p_down,
    stringsAsFactors = FALSE
  )
  attr(fcast, "yname") <- yname
  fcast

}

#' Logistic regression
#'
#' Standardized interface for logistic regression
#'
#' @param x Data frame with features.
#' @param y Binary vector indicating outcome event.
#'
#' @examples
#' credit_data <- recipes::credit_data
#'
#' mdl <- logistic_reg(credit_data[, setdiff(colnames(credit_data), "Status")],
#'                     credit_data$Status)
#'
#' @export
logistic_reg <- function(x, y) {

  if (inherits(y, "data.frame")) {
    y = y[[1]]
  }
  y   <- as.factor(y)

  df  <- dplyr::bind_cols(`.y` = y, x)
  fit <- stats::glm(.y ~ ., data = df, family = stats::binomial(link = "logit"))

  structure(
    list(model = fit),
    y_classes = levels(y),
    class = "logistic_reg"
  )
}

#' @export
predict.logistic_reg <- function(object, new_data, ...) {

  # ?glm states that for factor responses, the first level denotes failure
  # and all other levels denote success; so, predict(type = "response") will
  # gives us P for the second class
  y_classes <- attr(object, "y_classes")
  p <- predict(object$model, newdata = new_data, type = "response")
  preds <- tibble::tibble(
    p0 = 1 - p,
    p1 = p
  )
  colnames(preds) <- paste0("p_", y_classes)
  preds
}



