
credit_data <- recipes::credit_data
credit_data <- credit_data[complete.cases(credit_data), ]

credit_features <- credit_data[, setdiff(colnames(credit_data), "Status")]

test_that("feature extraction works", {

  expect_error(
    featx <- make_extract_features(credit_features),
    NA)
  expect_type(featx, "closure")

  expect_error(
    new_features <- featx(credit_features),
    NA
  )
  expect_s3_class(new_features, "data.frame")

})

test_that("logistic_reg_featx works", {

  expect_error(
    mdl <- logistic_reg_featx(credit_features, credit_data$Status),
    NA
  )
  expect_s3_class(mdl, "logistic_reg_featx")

})

test_that("predict.logistic_reg_featx works", {

  mdl <- logistic_reg_featx(credit_features, credit_data$Status)

  expect_error(
    preds <- predict(mdl, new_data = credit_data),
    NA
  )

  expect_equal(
    colnames(preds),
    paste0("p_", levels(credit_data$Status))
  )

})
