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

	# Create folds for cross fitting
	folds <- make_folds(cd@data, control@crossfit_folds)

	# Estimate nuisance parameters for \phi_n
	theta_ns <- foreach(i = 1:control@crossfit_folds,
											.combine = \(...) recombine_theta_n(..., folds = folds),
											.options.future = list(seed = TRUE)) %dofuture% {
		# Training
		train <- training(cd, folds, i)

		# Validation
		valid <- validation(cd, folds, i)

		theta_n(train, valid, cd@vars, learners_regressions, control)
	}

	alpha_ns <- foreach(i = 1:control@crossfit_folds,
											.combine = \(...) recombine_alpha_n(..., folds = folds)) %dofuture% {
		# Training
		train <- training(cd, folds, i)

		# Validation
		valid <- validation(cd, folds, i)

		list(
			"000" = phi_n_alpha(train, valid, nn_riesz_module, "data_0", "data_0", "data_0", control),
			"111" = phi_n_alpha(train, valid, nn_riesz_module, "data_1", "data_1", "data_1", control),
			"011" = phi_n_alpha(train, valid, nn_riesz_module, "data_0", "data_1", "data_1", control),
			"010" = phi_n_alpha(train, valid, nn_riesz_module, "data_0", "data_1", "data_0", control)
		)
	}

	eif_ns <- sapply(c("000", "111", "011", "010"), \(jkl) eif_n(cd, theta_ns, alpha_ns, jkl))

	# Estimates ---------------------------------------------------------------

	out <- list(
		# A -> Y
		p1 = mean(eif_ns[, "111"] - eif_ns[, "011"]),
		eif_p1 = eif_ns[, "111"] - eif_ns[, "011"],
		# A -> Z -> Y
		p2 = NA,
		eif_p2 = NA,
		# A -> Z -> M -> Y
		p3 = NA,
		eif_p3 = NA,
		# A -> M -> Y
		p4 = mean(eif_ns[, "010"] - eif_ns[, "000"]),
		eif_p4 = eif_ns[, "010"] - eif_ns[, "000"],
		# Intermediate confounding
		intermediate_confounding = NA,
		eif_intermediate_confounding = NA,
		ate = mean(eif_ns[, "111"] - eif_ns[, "000"]),
		eif_ate = eif_ns[, "111"] - eif_ns[, "000"]
	)

	class(out) <- "crumble"
	out
}
