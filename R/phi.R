phi_n_alpha <- function(train, valid, vars, architecture, params, control) {
	j <- params[1]
	k <- params[2]
	l <- params[3]

	.f1 <- \(alpha, dl) alpha(dl[[l]])
	.f2 <- \(alpha, dl) alpha(dl[[k]])
	.f3 <- \(alpha, dl) alpha(dl[[j]])

	alpha1 <- alpha_n(
		train = train,
		valid = valid,
		vars = na.omit(c(vars@A, vars@W)),
		architecture = architecture,
		.f = .f1,
		weights = NULL,
		control = control
	)

	alpha2 <- alpha_n(
		train = train,
		valid = valid,
		vars = na.omit(c(vars@A, vars@Z, vars@W)),
		architecture = architecture,
		.f = .f2,
		weights = alpha1$train,
		control = control
	)

	alpha3 <- alpha_n(
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

phi_r_alpha <- function(train, valid, vars, architecture, params, control) {
	i <- params[1]
	j <- params[2]
	k <- params[3]
	l <- params[4]


	.f1 <- \(alpha, data) alpha(data[[l]])
	.f2 <- \(alpha, data) alpha(data[[k]])
	.f3 <- \(alpha, data) alpha(data[[j]])
	.f4 <- \(alpha, data) alpha(data[[i]])

	alpha1 <- alpha_n(
		train = train,
		valid = valid,
		vars = na.omit(c(vars@A, vars@W)),
		architecture = architecture,
		.f = .f1,
		weights = NULL,
		control = control
	)

	alpha2 <- alpha_n(
		train = train,
		valid = valid,
		vars = na.omit(c(vars@A, vars@Z, vars@W)),
		architecture = architecture,
		.f = .f2,
		weights = alpha1$train,
		control = control
	)

	alpha3 <- alpha_n(
		train = train,
		valid = valid,
		vars = na.omit(c(vars@A, vars@M, vars@W)),
		architecture = architecture,
		.f = .f3,
		weights = alpha2$train,
		control = control
	)

	alpha4 <- alpha_n(
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
