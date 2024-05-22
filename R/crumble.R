crumble <- function(data,
										trt,
										outcome,
										mediators,
										moc,
										covar,
										cens = NULL,
										id = NULL,
										d0 = NULL,
										d1 = NULL,
										learners_regressions = "glm",
										nn_riesz_module = nn_sequential_architecture,
										control = crumble_control()) {

	# Perform initial checks
	checkmate::assert_data_frame(data[, c(trt, outcome, mediators, moc, covar, cens, id)])
	assert_not_missing(data, trt, covar, mediators, moc)
	checkmate::assert_function(d0, nargs = 2, null.ok = TRUE)
	checkmate::assert_function(d1, nargs = 2, null.ok = TRUE)
	checkmate::assert_function(nn_riesz_module)

	# Create crumble_data object
	cd <- crumble_data(
		data = data,
		vars = crumble_vars(
			A = trt,
			Y = outcome,
			M = mediators,
			Z = moc,
			W = covar,
			C = cens %??% NA_character_,
			id = id %??% NA_character_
		),
		d0 = d0,
		d1 = d1
	)

	# Create permuted Z
	cd@data_0zp <- cd@data_0
	cd@data_0zp[, cd@vars@Z] <- set_zp(cd)

	# Create folds for cross fitting
	folds <- make_folds(cd@data, control@crossfit_folds)

	# Estimate \theta nuisance parameters
	thetas <- foreach(i = 1:control@crossfit_folds,
										.combine = \(...) recombine_theta(..., folds = folds),
										.options.future = list(seed = TRUE)) %dofuture% {
		# Training
		train <- training(cd, folds, i)
		# Validation
		valid <- validation(cd, folds, i)

		theta(train, valid, cd@vars, learners_regressions, control)
	}

	alpha_ns <- foreach(i = 1:control@crossfit_folds,
											.combine = \(...) recombine_alpha(..., folds = folds)) %dofuture% {
		# Training
		train <- training(cd, folds, i)
		# Validation
		valid <- validation(cd, folds, i)

		list(
			"000" = phi_n_alpha(train, valid, cd@vars, nn_riesz_module, "data_0", "data_0", "data_0", control),
			"111" = phi_n_alpha(train, valid, cd@vars, nn_riesz_module, "data_1", "data_1", "data_1", control),
			"011" = phi_n_alpha(train, valid, cd@vars, nn_riesz_module, "data_0", "data_1", "data_1", control),
			"010" = phi_n_alpha(train, valid, cd@vars, nn_riesz_module, "data_0", "data_1", "data_0", control)
		)
	}

	alpha_rs <- foreach(i = 1:control@crossfit_folds,
											.combine = \(...) recombine_alpha(..., folds = folds)) %dofuture% {
		# Training
		train <- training(cd, folds, i)
		# Validation
		valid <- validation(cd, folds, i)

		list(
			"0111" = phi_r_alpha(train, valid, cd@vars, nn_riesz_module, "data_0zp", "data_1", "data_1", "data_1", control),
			"0011" = phi_r_alpha(train, valid, cd@vars, nn_riesz_module, "data_0zp", "data_0", "data_1", "data_1", control),
			"0010" = phi_r_alpha(train, valid, cd@vars, nn_riesz_module, "data_0zp", "data_0", "data_1", "data_0", control)
		)
	}

	eif_ns <- sapply(c("000", "111", "011", "010"), \(jkl) eif_n(cd, thetas$theta_n, alpha_ns, jkl))
	eif_rs <- sapply(c("0111", "0011", "0010"), \(ijkl) eif_r(cd, thetas$theta_r, alpha_rs, ijkl))

	# Estimates ---------------------------------------------------------------

	out <- list(
		# A -> Y
		p1 = mean(eif_ns[, "111"] - eif_ns[, "011"]),
		eif_p1 = eif_ns[, "111"] - eif_ns[, "011"],
		# A -> Z -> Y
		p2 = mean(eif_rs[, "0111"] - eif_rs[, "0011"]),
		eif_p2 = eif_rs[, "0111"] - eif_rs[, "0011"],
		# A -> Z -> M -> Y
		p3 = mean(eif_rs[, "0011"] - eif_rs[, "0010"]),
		eif_p3 = eif_rs[, "0011"] - eif_rs[, "0010"],
		# A -> M -> Y
		p4 = mean(eif_ns[, "010"] - eif_ns[, "000"]),
		eif_p4 = eif_ns[, "010"] - eif_ns[, "000"],
		# Intermediate confounding
		intermediate_confounding = mean(
			eif_ns[, "011"] - eif_rs[, "0111"] +
				eif_rs[, "0011"] - eif_rs[, "0011"] +
				eif_rs[, "0010"] - eif_ns[, "010"]
		),
		eif_intermediate_confounding =
			eif_ns[, "011"] - eif_rs[, "0111"] +
			eif_rs[, "0011"] - eif_rs[, "0011"] +
			eif_rs[, "0010"] - eif_ns[, "010"],
		ate = mean(eif_ns[, "111"] - eif_ns[, "000"]),
		eif_ate = eif_ns[, "111"] - eif_ns[, "000"]
	)

	class(out) <- "crumble"
	out
}
