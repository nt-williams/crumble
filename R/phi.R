phi_n_alpha <- function(train, valid, vars, architecture, j, k, l, control) {
	if (!no_Z(vars)) {
		.f1 <- \(alpha, data) alpha(as_torch(one_hot_encode(data[[l]][, na.omit(c(vars@A, vars@C, vars@W))])))
		.f2 <- \(alpha, data) alpha(as_torch(one_hot_encode(data[[k]][, na.omit(c(vars@A, vars@C, vars@Z, vars@W))])))
		.f3 <- \(alpha, data) alpha(as_torch(one_hot_encode(data[[j]][, na.omit(c(vars@A, vars@C, vars@M, vars@Z, vars@W))])))
	} else {
		.f1 <- \(alpha, data) alpha(as_torch(one_hot_encode(data[[k]][, na.omit(c(vars@A, vars@C, vars@W))])))
		.f3 <- \(alpha, data) alpha(as_torch(one_hot_encode(data[[j]][, na.omit(c(vars@A, vars@C, vars@M, vars@W))])))
	}

	alpha1 <- alpha_n(
		train = train,
		valid = valid,
		vars = na.omit(c(vars@A, vars@C, vars@W)),
		architecture = architecture,
		.f = .f1,
		weights = NULL,
		control = control
		)

	if (!no_Z(vars)) {
		alpha2 <- alpha_n(
			train = train,
			valid = valid,
			vars = na.omit(c(vars@A, vars@C, vars@Z, vars@W)),
			architecture = architecture,
			.f = .f2,
			weights = alpha1$train,
			control = control
		)
	} else {
		alpha2 <- alpha1
	}

	alpha3 <- alpha_n(
		train = train,
		valid = valid,
		vars = na.omit(c(vars@A, vars@C, vars@M, vars@Z, vars@W)),
		architecture = architecture,
		.f = .f3,
		weights = alpha2$train,
		control = control
	)

	if (!no_Z(vars)) {
		list(jkl = gsub("data_", "", paste0(j, k, l, collapse = "")),
				 alpha1 = alpha1$valid,
				 alpha2 = alpha2$valid,
				 alpha3 = alpha3$valid)
	} else {
		list(jk = gsub("data_", "", paste0(j, k, collapse = "")),
				 alpha1 = alpha1$valid,
				 alpha2 = alpha3$valid)
	}
}

phi_r_alpha <- function(train, valid, vars, architecture, i, j, k, l, control) {
	.f1 <- \(alpha, data) alpha(as_torch(one_hot_encode(data[[l]][, na.omit(c(vars@A, vars@C, vars@W))])))
	.f2 <- \(alpha, data) alpha(as_torch(one_hot_encode(data[[k]][, na.omit(c(vars@A, vars@C, vars@Z, vars@W))])))
	.f3 <- \(alpha, data) alpha(as_torch(one_hot_encode(data[[j]][, na.omit(c(vars@A, vars@C, vars@M, vars@W))])))
	.f4 <- \(alpha, data) alpha(as_torch(one_hot_encode(data[[i]][, na.omit(c(vars@A, vars@C, vars@Z, vars@M, vars@W))])))

	alpha1 <- alpha_n(
		train = train,
		valid = valid,
		vars = na.omit(c(vars@A, vars@C, vars@W)),
		architecture = architecture,
		.f = .f1,
		weights = NULL,
		control = control
	)

	alpha2 <- alpha_n(
		train = train,
		valid = valid,
		vars = na.omit(c(vars@A, vars@C, vars@Z, vars@W)),
		architecture = architecture,
		.f = .f2,
		weights = alpha1$train,
		control = control
	)

	alpha3 <- alpha_n(
		train = train,
		valid = valid,
		vars = na.omit(c(vars@A, vars@C, vars@M, vars@W)),
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
