theta <- function(train, valid, vars, learners, control) {
	continuous <- !is_binary(train$data[[vars@Y]])
	valid <- valid[sapply(valid, \(x) ncol(x) > 0)]

	theta_y <- mlr3superlearner::mlr3superlearner(
		data = train$data[, na.omit(c(vars@A, vars@W, vars@M, vars@Z, vars@Y))],
		target = vars@Y,
		library = learners,
		outcome_type = ifelse(continuous, "continuous", "binomial"),
		folds = control@mlr3superlearner_folds,
		newdata = valid,
		group = NULL
	)

	# \theta_n ----------------------------------------------------------------

	if (!no_Z(vars)) {
		params <- list(c("data_0", "data_0", "data_0"),
									 c("data_1", "data_1", "data_1"),
									 c("data_0", "data_1", "data_1"),
									 c("data_0", "data_1", "data_0"))
		params <- lapply(params, \(x) setNames(x, c("j", "k", "l")))
		vals_n <- vector("list", length = 4)
		names(vals_n) <- c("000", "111", "011", "010")
	} else {
		params <- list(c("data_0", "data_0"),
									 c("data_1", "data_1"),
									 c("data_0", "data_1"))
		params <- lapply(params, \(x) setNames(x, c("j", "k")))
		vals_n <- vector("list", length = 3)
		names(vals_n) <- c("00", "11", "01")
	}

	for (s in seq_along(params)) {
		j <- params[[s]]["j"]
		k <- params[[s]]["k"]
		l <- params[[s]]["l"]

		if (!no_Z(vars)) {
			b3_train <- predict(theta_y, train[[j]])
			b3_valid <- theta_y$preds[[j]]
		} else {
			b2_train <- predict(theta_y, train[[j]])
			b2_valid <- theta_y$preds[[j]]
		}

		if (!no_Z(vars)) {
			theta2 <- mlr3superlearner::mlr3superlearner(
				data = add_psuedo(train$data[, c(vars@A, vars@W, vars@Z)], b3_train),
				target = "tmp_crumble_pseudo_y",
				library = learners,
				outcome_type = "continuous",
				folds = control@mlr3superlearner_folds,
				newdata = valid,
				group = NULL
			)

			b2_train <- predict(theta2, train[[k]])
			b2_valid <- theta2$preds[[k]]
		}

		theta1 <- mlr3superlearner::mlr3superlearner(
			data = add_psuedo(train$data[, c(vars@A, vars@W)], b2_train),
			target = "tmp_crumble_pseudo_y",
			library = learners,
			outcome_type = "continuous",
			folds = control@mlr3superlearner_folds,
			newdata = valid,
			group = NULL
		)

		if (!no_Z(vars)) {
			vals_n[[s]] <- list(
				fit3_weights = theta_y$weights,
				fit3_natural = theta_y$preds$data,
				b3 = b3_valid,
				fit2_weights = theta2$weights,
				fit2_natural = theta2$preds$data,
				b2 = b2_valid,
				fit1_weights = theta1$weights,
				fit1_natural = theta1$preds$data,
				b1 = theta1$preds[[l]]
			)
		} else {
			vals_n[[s]] <- list(
				fit2_weights = theta_y$weights,
				fit2_natural = theta_y$preds$data,
				b2 = b2_valid,
				fit1_weights = theta1$weights,
				fit1_natural = theta1$preds$data,
				b1 = theta1$preds[[k]]
			)
		}
	}

	if (no_Z(vars)) {
		return(list(n = vals_n))
	}

	# \theta_r ----------------------------------------------------------------

	params <- list(c("data_0zp", "data_1", "data_1", "data_1"),
								 c("data_0zp", "data_0", "data_1", "data_1"),
								 c("data_0zp", "data_0", "data_1", "data_0"))
	params <- lapply(params, \(x) setNames(x, c("i", "j", "k", "l")))

	vals_r <- vector("list", length = 3)
	for (s in seq_along(params)) {
		i <- params[[s]]["i"]
		j <- params[[s]]["j"]
		k <- params[[s]]["k"]
		l <- params[[s]]["l"]

		b4_train <- predict(theta_y, train[[i]])
		b4_valid <- theta_y$preds[[i]]

		theta3 <- mlr3superlearner::mlr3superlearner(
			data = add_psuedo(train$data[, c(vars@A, vars@W, vars@M)], b4_train),
			target = "tmp_crumble_pseudo_y",
			library = learners,
			outcome_type = "continuous",
			folds = control@mlr3superlearner_folds,
			newdata = valid,
			group = NULL
		)

		b3_train <- predict(theta3, train[[j]])
		b3_valid <- theta3$preds[[j]]

		theta2 <- mlr3superlearner::mlr3superlearner(
			data = add_psuedo(train$data[, c(vars@A, vars@W, vars@Z)], b3_train),
			target = "tmp_crumble_pseudo_y",
			library = learners,
			outcome_type = "continuous",
			folds = control@mlr3superlearner_folds,
			newdata = valid,
			group = NULL
		)

		b2_train <- predict(theta2, train[[k]])
		b2_valid <- theta2$preds[[k]]

		theta1 <- mlr3superlearner::mlr3superlearner(
			data = add_psuedo(train$data[, c(vars@A, vars@W)], b2_train),
			target = "tmp_crumble_pseudo_y",
			library = learners,
			outcome_type = "continuous",
			folds = control@mlr3superlearner_folds,
			newdata = valid,
			group = NULL
		)

		vals_r[[s]] <- list(
			fit4_weights = theta_y$weights,
			fit4_natural = theta_y$preds$data,
			b4 = b4_valid,
			fit3_weights = theta3$weights,
			fit3_natural = theta3$preds$data,
			b3 = b3_valid,
			fit2_weights = theta2$weights,
			fit2_natural = theta2$preds$data,
			b2 = b2_valid,
			fit1_weights = theta1$weights,
			fit1_natural = theta1$preds$data,
			b1 = theta1$preds[[l]]
		)
	}

	names(vals_r) <- c("0111", "0011", "0010")

	list(n = vals_n,
			 r = vals_r)
}
