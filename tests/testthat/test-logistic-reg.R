
test_that("logist_reg works", {

  credit_data <- recipes::credit_data
  expect_error(
    mdl <- logistic_reg(credit_data[, setdiff(colnames(credit_data), "Status")],
                        credit_data$Status),
    NA
  )

})

test_that("predict.logistic_reg works", {

  credit_data <- recipes::credit_data
  mdl <- logistic_reg(credit_data[, setdiff(colnames(credit_data), "Status")],
                      credit_data$Status)
  expect_error(
    preds <- predict(mdl, new_data = credit_data),
    NA
  )

})

test_that("ds_logistic_reg work", {

  data("states")

  expect_error(
    mdl <- ds_logistic_reg("v2x_veracc_osp", states),
    NA
  )


})

test_that("predict.ds_logitst_reg works", {

  data("states")
  mdl <- ds_logistic_reg("v2x_veracc_osp", states)

  expect_error(
    preds <- predict(mdl, new_data = states),
    NA
  )


})
