library(dplyr)

data <- gendata(500)

crumble(data, "a", "y", "m", "z", "w",
				d0 = lmtp::static_binary_off,
				d1 = lmtp::static_binary_on)

data <- gendata2(1e4)

crumble(data, "A", "Y", c("M_1", "M_2"), c("Z_1", "Z_2"), paste0("W_", 1:3),
				d0 = lmtp::static_binary_off,
				d1 = lmtp::static_binary_on,
				learners_regressions = c("glm", "earth", "ranger"),
				control = crumble_control(crossfit_folds = 2L, mlr3superlearner_folds = 2L))
