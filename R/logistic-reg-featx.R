
#' Logistic regression with feature extraction
#'
#' Logistic regression with a feature extraction step applied to the training
#' data, e.g. PCA.
#'
#' @param space (`character(1)`) \cr
#'   Name of the V-Dem indicator for a democratic space
#' @param data ([base::data.frame()])
#'   A data frame or similar object
#'
#' @examples
#' data("states")
#'
#' #mdl <- ds_logistic_reg_pca("v2x_veracc_osp", states)
#' #preds <- predict(mdl, new_data = states)
#'
#' @export
ds_logistic_reg_featx <- function(space, data) {

}


#' @export
#' @importFrom stats predict
predict.ds_logistic_reg_featx <- function(object, new_data, ...) {

}

#' Logistic regression with feature extraction
#'
#' Standardized interface for logistic regression with a feature extraction
#' step, e.g. PCA.
#'
#' @param x Data frame with features.
#' @param y Binary vector indicating outcome event.
#'
#' @examples
#' credit_data <- recipes::credit_data
#' credit_data <- credit_data[complete.cases(credit_data), ]
#'
#' mdl <- logistic_reg_featx(
#'   credit_data[, setdiff(colnames(credit_data), "Status")],
#'   credit_data$Status)
#'
#' @export
logistic_reg_featx <- function(x, y) {

  stopifnot(all(stats::complete.cases(x)),
            all(!is.na(y)))

  extract_features <- make_extract_features(x)

  x_new <- extract_features(x)

  mdl <- logistic_reg(x_new, y)
  mdl$extract_features <- extract_features
  class(mdl) <- c("logistic_reg_featx", class(mdl))
  mdl
}

#' Make feature extractor
#'
#' Creates a feature extractor function
#'
#' @param x a [base::data.frame()] or similar object with only feature variables
#'
#' @examples
#' library("dplyr")
#' library("stats")
#' data("states")
#'
#' # assume that the input consists entirely of features, and no NAs
#' train_x <- states %>%
#'   filter(year < 2010) %>%
#'   select(-starts_with("dv_"), -gwcode, -year) %>%
#'   filter(complete.cases(.))
#'
#' test_x <- states %>%
#'   filter(year > 2009) %>%
#'   select(-starts_with("dv_"), -gwcode, -year) %>%
#'   filter(complete.cases(.))
#'
#' featx <- make_extract_features(train_x)
#' featx(train_x)
#'
#' @export
make_extract_features <- function(x) {

  featx <- recipes::recipe( ~ ., data = x) %>%
    recipes::step_normalize(recipes::all_numeric()) %>%
    recipes::step_pca(recipes::all_numeric())
  # we want to fit the PCA parameters using the x data, and re-use it later
  # for prediction
  # Warning: All elements of `...` must be named.
  # this is from https://github.com/tidymodels/recipes/pull/364
  suppressWarnings(
    featx_fitted <- recipes::prep(featx, training = x)
  )

  function(x) {
    # Warning: All elements of `...` must be named.
    # this is from https://github.com/tidymodels/recipes/pull/364
    suppressWarnings(
      recipes::bake(featx_fitted, new_data = x)
    )
  }
}


#' @export
predict.logistic_reg_featx <- function(object, new_data, ...) {

  features <- object$extract_features(new_data)
  # this will go to predict.logistic_reg, but using the process data
  NextMethod(new_data = features)

}
