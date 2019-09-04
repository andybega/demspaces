
#' Logistic regression with PCA filter
#'
#' Logistic regression with a PCA pre-processing step to reduce feature
#' dimensionality.
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
ds_logistic_reg_pca <- function(space, data) {

}


#' @export
#' @importFrom stats predict
predict.ds_logistic_reg_pca <- function(object, new_data, ...) {

}

#' Logistic regression with PCA
#'
#' Standardized interface for logistic regression with PCA feature variable
#' dimensionality reduction.
#'
#' @param x Data frame with features.
#' @param y Binary vector indicating outcome event.
#'
#' @examples
#' credit_data <- recipes::credit_data
#'
#' #mdl <- logistic_reg(credit_data[, setdiff(colnames(credit_data), "Status")],
#' #                     credit_data$Status)
#'
#' @export
logistic_reg_pca <- function(x, y) {

  NULL

}


#' @export
predict.logistic_reg <- function(object, new_data, ...) {

}
