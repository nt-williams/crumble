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

	.f1 <- \(alpha, dl) alpha(dl[[l]])
	.f2 <- \(alpha, dl) alpha(dl[[k]])
	.f3 <- \(alpha, dl) alpha(dl[[j]])

	alpha1 <- Alpha(
		train = train,
		valid = valid,
		vars = na.omit(c(vars@A, vars@W)),
		architecture = architecture,
		.f = .f1,
		weights = NULL,
		control = control
	)

	alpha2 <- Alpha(
		train = train,
		valid = valid,
		vars = na.omit(c(vars@A, vars@Z, vars@W)),
		architecture = architecture,
		.f = .f2,
		weights = alpha1$train,
		control = control
	)

	alpha3 <- Alpha(
		train = train,
		valid = valid,
		vars = na.omit(c(vars@A, vars@C, vars@M, vars@Z, vars@W)),
		architecture = architecture,
		.f = .f3,
		weights = alpha2$train,
		control = control
	)

	list(jkl = gsub("data_", "", paste0(j, k, l, collapse = "")),
			 alpha1 = alpha1$valid,
			 alpha2 = alpha2$valid,
			 alpha3 = alpha3$valid)
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


	.f1 <- \(alpha, data) alpha(data[[l]])
	.f2 <- \(alpha, data) alpha(data[[k]])
	.f3 <- \(alpha, data) alpha(data[[j]])
	.f4 <- \(alpha, data) alpha(data[[i]])

	alpha1 <- Alpha(
		train = train,
		valid = valid,
		vars = na.omit(c(vars@A, vars@W)),
		architecture = architecture,
		.f = .f1,
		weights = NULL,
		control = control
	)

	alpha2 <- Alpha(
		train = train,
		valid = valid,
		vars = na.omit(c(vars@A, vars@Z, vars@W)),
		architecture = architecture,
		.f = .f2,
		weights = alpha1$train,
		control = control
	)

	alpha3 <- Alpha(
		train = train,
		valid = valid,
		vars = na.omit(c(vars@A, vars@M, vars@W)),
		architecture = architecture,
		.f = .f3,
		weights = alpha2$train,
		control = control
	)

	alpha4 <- Alpha(
		train = train,
		valid = valid,
		vars = na.omit(c(vars@A, vars@C, vars@Z, vars@M, vars@W)),
		architecture = architecture,
		.f = .f4,
		weights = alpha3$train,
		control = control
	)

	list(ijkl = gsub("data_", "", paste0(i, j, k, l, collapse = "")),
			 alpha1 = alpha1$valid,
			 alpha2 = alpha2$valid,
			 alpha3 = alpha3$valid,
			 alpha4 = alpha4$valid)
}
