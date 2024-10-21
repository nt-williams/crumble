estimate_phi_n_alpha <- function(cd, folds, params, nn_module, control) {
	if (length(params$natural) != 0) {
		alpha_ns <- vector("list", control$crossfit_folds)
		i <- 1
		cli::cli_progress_step("Computing alpha n density ratios... {i}/{control$crossfit_folds} folds")
		for (i in seq_along(folds)) {
			train <- training(cd, folds, i)
			valid <- validation(cd, folds, i)

			alpha_ns[[i]] <- lapply(
				params$natural,
				\(param) phi_n_alpha(train, valid, cd@vars, nn_module, param, control)
			)

			names(alpha_ns[[i]]) <- unlist(lapply(params$natural, \(x) paste0(gsub("data_", "", x), collapse = "")))
			cli::cli_progress_update()
		}

		cli::cli_progress_done()
		return(recombine_alpha(alpha_ns, folds))
	}
	NULL
}

phi_n_alpha <- function(train, valid, vars, architecture, params, control) {
	j <- params[1]
	k <- params[2]
	l <- params[3]

	alpha1 <- rieszboost::rieszboost(
		data = train$data,
		A = vars@A,
		X = vars@W,
		shifted = train[[l]],
		newdata = valid$data,
		rounds = 500
	)

	alpha2 <- rieszboost::rieszboost(
		data = train$data,
		A = vars@A,
		X = na.omit(c(vars@Z, vars@W)),
		shifted = train[[k]],
		newdata = valid$data,
		weights = alpha1$fitted,
		rounds = 500
	)

	alpha3 <- rieszboost::rieszboost(
		data = train$data,
		A = vars@A,
		X = na.omit(c(vars@C, vars@M, vars@Z, vars@W)),
		shifted = train[[j]],
		newdata = valid$data,
		weights = alpha2$fitted,
		rounds = 500
	)

	list(jkl = gsub("data_", "", paste0(j, k, l, collapse = "")),
			 alpha1 = alpha1$pred,
			 alpha2 = alpha2$pred,
			 alpha3 = alpha3$pred)
}

estimate_phi_r_alpha <- function(cd, folds, params, nn_module, control) {
	if (length(params$randomized) != 0) {
		alpha_rs <- vector("list", control$crossfit_folds)
		i <- 1
		cli::cli_progress_step("Computing alpha r density ratios... {i}/{control$crossfit_folds} folds")
		for (i in seq_along(folds)) {
			train <- training(cd, folds, i)
			valid <- validation(cd, folds, i)

			alpha_rs[[i]] <- lapply(
				params$randomized,
				\(param) phi_r_alpha(train, valid, cd@vars, nn_module, param, control)
			)

			names(alpha_rs[[i]]) <-
				gsub("zp", "", unlist(lapply(params$randomized, \(x) paste0(gsub("data_", "", x), collapse = ""))))

			cli::cli_progress_update()
		}
		return(recombine_alpha(alpha_rs, folds))
	}
	NULL
}

phi_r_alpha <- function(train, valid, vars, architecture, params, control) {
	i <- params[1]
	j <- params[2]
	k <- params[3]
	l <- params[4]

	alpha1 <- rieszboost::rieszboost(
		data = train$data,
		A = vars@A,
		X = vars@W,
		shifted = train[[l]],
		newdata = valid$data,
		rounds = 500
	)

	alpha2 <- rieszboost::rieszboost(
		data = train$data,
		A = vars@A,
		X = na.omit(c(vars@Z, vars@W)),
		shifted = train[[k]],
		newdata = valid$data,
		weights = alpha1$fitted,
		rounds = 500
	)

	alpha3 <- rieszboost::rieszboost(
		data = train$data,
		A = vars@A,
		X = na.omit(c(vars@M, vars@W)),
		shifted = train[[j]],
		newdata = valid$data,
		weights = alpha2$fitted,
		rounds = 500
	)

	alpha4 <- rieszboost::rieszboost(
		data = train$data,
		A = vars@A,
		X = na.omit(c(vars@C, vars@Z, vars@M, vars@W)),
		shifted = train[[i]],
		newdata = valid$data,
		weights = alpha3$fitted,
		rounds = 500
	)

	list(ijkl = gsub("data_", "", paste0(i, j, k, l, collapse = "")),
			 alpha1 = alpha1$pred,
			 alpha2 = alpha2$pred,
			 alpha3 = alpha3$pred,
			 alpha4 = alpha4$pred)
}
