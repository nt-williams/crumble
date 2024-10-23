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

	net <- make_module(train$data[, c(vars@W, vars@A), drop = FALSE])
	alpha1 <- riesznet::riesznet(
		data = train$data[, c(vars@W, vars@A), drop = FALSE],
		shifted = list(data_1 = train[[l]][, c(vars@W, vars@A), drop = FALSE]),
		.f = \(data_1) data_1,
		net = net,
		epochs = control$epochs,
		max_lr = control$learning_rate,
		batch_size = control$batch_size,
		weight_decay = control$weight_decay,
		patience = control$patience,
		verbose = TRUE
	)

	net <- make_module(train$data[, na.omit(c(vars@Z, vars@W, vars@A)), drop = FALSE])
	alpha2 <- riesznet::riesznet(
		data = train$data[, na.omit(c(vars@Z, vars@W, vars@A)), drop = FALSE],
		shifted = list(data_1 = train[[k]][, na.omit(c(vars@Z, vars@W, vars@A)), drop = FALSE]),
		.f = \(data_1) data_1,
		net = net,
		weights = as.numeric(predict(alpha1, train$data)),
		epochs = control$epochs,
		max_lr = control$learning_rate,
		batch_size = control$batch_size,
		weight_decay = control$weight_decay,
		patience = control$patience,
		verbose = TRUE
	)

	net <- make_module(train$data[, na.omit(c(vars@C, vars@M, vars@Z, vars@W)), drop = FALSE])
	alpha3 <- riesznet::riesznet(
		data = train$data[, na.omit(c(vars@C, vars@M, vars@Z, vars@W)), drop = FALSE],
		shifted = list(data_1 = train[[j]][, na.omit(c(vars@C, vars@M, vars@Z, vars@W)), drop = FALSE]),
		.f = \(data_1) data_1,
		net = net,
		weights = as.numeric(predict(alpha2, train$data)),
		epochs = control$epochs,
		max_lr = control$learning_rate,
		batch_size = control$batch_size,
		weight_decay = control$weight_decay,
		patience = control$patience,
		verbose = TRUE
	)

	list(jkl = gsub("data_", "", paste0(j, k, l, collapse = "")),
			 alpha1 = as.numeric(predict(alpha1, valid$data)),
			 alpha2 = as.numeric(predict(alpha2, valid$data)),
			 alpha3 = as.numeric(predict(alpha3, valid$data)))
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
				gsub("zp", "", unlist(lapply(
					params$randomized, \(x) paste0(gsub("data_", "", x), collapse = "")
				)))

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

	net <- make_module(train$data[, c(vars@W, vars@A), drop = FALSE])
	alpha1 <- riesznet::riesznet(
		data = train$data[, c(vars@W, vars@A), drop = FALSE],
		shifted = list(data_1 = train[[l]][, c(vars@W, vars@A), drop = FALSE]),
		.f = \(data_1) data_1,
		net = net,
		epochs = control$epochs,
		max_lr = control$learning_rate,
		batch_size = control$batch_size,
		weight_decay = control$weight_decay,
		patience = control$patience,
		verbose = TRUE
	)

	net <- make_module(train$data[, na.omit(c(vars@Z, vars@W, vars@A)), drop = FALSE])
	alpha2 <- riesznet::riesznet(
		data = train$data[, na.omit(c(vars@Z, vars@W, vars@A)), drop = FALSE],
		shifted = list(data_1 = train[[k]][, na.omit(c(vars@Z, vars@W, vars@A)), drop = FALSE]),
		.f = \(data_1) data_1,
		net = net,
		weights = as.numeric(predict(alpha1, train$data)),
		epochs = control$epochs,
		max_lr = control$learning_rate,
		batch_size = control$batch_size,
		weight_decay = control$weight_decay,
		patience = control$patience,
		verbose = TRUE
	)

	net <- make_module(train$data[, na.omit(c(vars@M, vars@W, vars@A)), drop = FALSE])
	alpha3 <- riesznet::riesznet(
		data = train$data[, na.omit(c(vars@M, vars@W, vars@A)), drop = FALSE],
		shifted = list(data_1 = train[[j]][, na.omit(c(vars@M, vars@W, vars@A)), drop = FALSE]),
		.f = \(data_1) data_1,
		net = net,
		weights = as.numeric(predict(alpha2, train$data)),
		epochs = control$epochs,
		max_lr = control$learning_rate,
		batch_size = control$batch_size,
		weight_decay = control$weight_decay,
		patience = control$patience,
		verbose = TRUE
	)

	net <- make_module(train$data[, na.omit(c(vars@C, vars@Z, vars@M, vars@W, vars@A)), drop = FALSE])
	alpha4 <- riesznet::riesznet(
		data = train$data[, na.omit(c(vars@C, vars@Z, vars@M, vars@W, vars@A)), drop = FALSE],
		shifted = list(data_1 = train[[i]][, na.omit(c(vars@C, vars@Z, vars@M, vars@W, vars@A)), drop = FALSE]),
		.f = \(data_1) data_1,
		net = net,
		weights = as.numeric(predict(alpha3, train$data)),
		epochs = control$epochs,
		max_lr = control$learning_rate,
		batch_size = control$batch_size,
		weight_decay = control$weight_decay,
		patience = control$patience,
		verbose = TRUE
	)

	list(ijkl = gsub("data_", "", paste0(i, j, k, l, collapse = "")),
			 alpha1 = as.numeric(predict(alpha1, valid$data)),
			 alpha2 = as.numeric(predict(alpha2, valid$data)),
			 alpha3 = as.numeric(predict(alpha3, valid$data)),
			 alpha4 = as.numeric(predict(alpha4, valid$data)))
}
