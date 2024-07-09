theta <- function(train, valid, vars, params, learners, control) {
	continuous <- !is_binary(train$data[[vars@Y]])
	valid <- valid[sapply(valid, \(x) ncol(x) > 0)]
	obs <- censored(train$data, vars@C)

	theta_y <- mlr3superlearner::mlr3superlearner(
		data = train$data[obs, na.omit(c(vars@A, vars@W, vars@M, vars@Z, vars@Y))],
		target = vars@Y,
		library = learners,
		outcome_type = ifelse(continuous, "continuous", "binomial"),
		folds = control$mlr3superlearner_folds,
		newdata = valid,
		group = NULL
	)

	# Natural -----------------------------------------------------------------

	if (length(params$natural) != 0) {
		vals_n <- vector("list", length = length(params$natural))
		names(vals_n) <- unlist(lapply(params$natural, \(x) paste0(gsub("data_", "", x), collapse = "")))

		for (s in seq_along(params$natural)) {
			j <- params$natural[[s]]["j"]
			k <- params$natural[[s]]["k"]
			l <- params$natural[[s]]["l"]

			b3_train <- predict(theta_y, train[[j]])
			b3_valid <- theta_y$preds[[j]]

			theta2 <- mlr3superlearner::mlr3superlearner(
				data = add_psuedo(train$data[, na.omit(c(vars@A, vars@W, vars@Z))], b3_train),
				target = "tmp_crumble_pseudo_y",
				library = learners,
				outcome_type = "continuous",
				folds = control$mlr3superlearner_folds,
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
				folds = control$mlr3superlearner_folds,
				newdata = valid,
				group = NULL
			)

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
		}

		if (length(params$randomized) == 0) {
			return(list(n = vals_n))
		}
	}

	# Randomized --------------------------------------------------------------

	vals_r <- vector("list", length = length(params$randomized))
	for (s in seq_along(params$randomized)) {
		i <- params$randomized[[s]]["i"]
		j <- params$randomized[[s]]["j"]
		k <- params$randomized[[s]]["k"]
		l <- params$randomized[[s]]["l"]

		b4_train <- predict(theta_y, train[[i]])
		b4_valid <- theta_y$preds[[i]]

		theta3 <- mlr3superlearner::mlr3superlearner(
			data = add_psuedo(train$data[, c(vars@A, vars@W, vars@M)], b4_train),
			target = "tmp_crumble_pseudo_y",
			library = learners,
			outcome_type = "continuous",
			folds = control$mlr3superlearner_folds,
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
			folds = control$mlr3superlearner_folds,
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
			folds = control$mlr3superlearner_folds,
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

	names(vals_r) <-
		gsub("zp", "", unlist(lapply(params$randomized, \(x) paste0(gsub("data_", "", x), collapse = ""))))

	if (length(params$natural) == 0) {
		return(list(r = vals_r))
	}

	list(n = vals_n,
			 r = vals_r)
}
