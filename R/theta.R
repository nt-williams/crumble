theta <- function(train, valid, vars, learners, control) {
	continuous <- !is_binary(train$data[[vars@Y]])
	valid <- valid[sapply(valid, \(x) ncol(x) > 0)]

	theta_y <- crossfit(
		train = train$data[, na.omit(c(vars@A, vars@W, vars@M, vars@Z, vars@Y))],
		valid = valid,
		y = vars@Y,
		continuous = continuous,
		id = NULL,
		learners = learners,
		folds = control@mlr3superlearner_folds,
		bound = FALSE
	)

	# \theta_n ----------------------------------------------------------------

	if (!is.na(vars@Z)) {
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

		if (!is.na(vars@Z)) {
			b3 <- theta_y$preds[[j]]
		} else {
			b2 <- theta_y$preds[[j]]
		}

		if (!is.na(vars@Z)) {
			theta2 <- crossfit(
				train = add_psuedo(train$data[, c(vars@A, vars@W, vars@Z)], b3),
				valid = valid,
				y = "tmp_crumble_pseudo_y",
				continuous = TRUE,
				id = NULL,
				learners = learners,
				folds = control@mlr3superlearner_folds,
				bound = FALSE
			)

			b2 <- theta2$preds[[k]]
		}

		theta1 <- crossfit(
			train = add_psuedo(train$data[, c(vars@A, vars@W)], b2),
			valid = valid,
			y = "tmp_crumble_pseudo_y",
			continuous = TRUE,
			id = NULL,
			learners = learners,
			folds = control@mlr3superlearner_folds,
			bound = FALSE
		)

		if (!is.na(vars@Z)) {
			b1 <- theta1$preds[[l]]

			vals_n[[s]] <- list(
				fit3_weights = theta_y$weights,
				fit3_natural = theta_y$preds$data,
				b3 = b3,
				fit2_weights = theta2$weights,
				fit2_natural = theta2$preds$data,
				b2 = b2,
				fit1_weights = theta1$weights,
				fit1_natural = theta1$preds$data,
				b1 = b1
			)
		} else {
			b1 <- theta1$preds[[k]]

			vals_n[[s]] <- list(
				fit2_weights = theta_y$weights,
				fit2_natural = theta_y$preds$data,
				b2 = b2,
				fit1_weights = theta1$weights,
				fit1_natural = theta1$preds$data,
				b1 = b1
			)
		}
	}

	if (is.na(vars@Z)) {
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

		b4 <- theta_y$preds[[i]]

		theta3 <- crossfit(
			train = add_psuedo(train$data[, c(vars@A, vars@W, vars@M)], b4),
			valid = valid,
			y = "tmp_crumble_pseudo_y",
			continuous = TRUE,
			id = NULL,
			learners = learners,
			folds = control@mlr3superlearner_folds,
			bound = FALSE
		)

		b3 <- theta3$preds[[j]]

		theta2 <- crossfit(
			train = add_psuedo(train$data[, c(vars@A, vars@W, vars@Z)], b3),
			valid = valid,
			y = "tmp_crumble_pseudo_y",
			continuous = TRUE,
			id = NULL,
			learners = learners,
			folds = control@mlr3superlearner_folds,
			bound = FALSE
		)

		b2 <- theta2$preds[[k]]

		theta1 <- crossfit(
			train = add_psuedo(train$data[, c(vars@A, vars@W)], b2),
			valid = valid,
			y = "tmp_crumble_pseudo_y",
			continuous = TRUE,
			id = NULL,
			learners = learners,
			folds = control@mlr3superlearner_folds,
			bound = FALSE
		)

		b1 <- theta1$preds[[l]]

		vals_r[[s]] <- list(
			fit4_weights = theta_y$weights,
			fit4_natural = theta_y$preds$data,
			b4 = b4,
			fit3_weights = theta3$weights,
			fit3_natural = theta3$preds$data,
			b3 = b3,
			fit2_weights = theta2$weights,
			fit2_natural = theta2$preds$data,
			b2 = b2,
			fit1_weights = theta1$weights,
			fit1_natural = theta1$preds$data,
			b1 = b1
		)
	}

	names(vals_r) <- c("0111", "0011", "0010")

	list(n = vals_n,
			 r = vals_r)
}
