# demspaces 0.0.1.9000

* Added a `NEWS.md` file to track changes to the package.

## New models

* `logistic_reg()` is a standard logistic regression model, with `ds_logistic_reg()` as a wrapper for modeling democratic spaces. It includes an option to standardize features prior to model estimation ("normalize" argument), using a standardizer function made by `make_standardizer()`. 
* `logistic_reg_featx()` and `ds_logistic_reg_featx()` are standard logistic regression models with a feature extraction pre-processing step for the input feature data. This uses PCA (the only method implemented currently) to reduce the number of numeric input features to 5, via `make_extract_features()`. 
