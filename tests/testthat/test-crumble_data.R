source("tests/testthat/helpers.R")

dat <- gendata(100)

vars <- crumble_vars("a", "y", "m", "z", "w")
cd <- crumble_data(dat, vars, lmtp::static_binary_on, lmtp::static_binary_off)

fold_obj <- make_folds(cd@data, 5)

training(cd, fold_obj, 1)
validation(cd, fold_obj, 1)

vars <- crumble_vars("a", "y", "m", "z", "f")
cd <- crumble_data(dat, vars, lmtp::static_binary_on, lmtp::static_binary_off)
