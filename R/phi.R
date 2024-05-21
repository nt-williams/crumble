phi_n_alpha <- function(train, valid, architecture, j, k, l, control) {
	.f1 <- \(alpha, data) alpha(as_torch(data[[j]][, c("a", "w")]))
	.f2 <- \(alpha, data) alpha(as_torch(data[[k]][, c("a", "z", "w")]))
	.f3 <- \(alpha, data) alpha(as_torch(data[[l]][, c("a", "m", "z", "w")]))

	alpha1 <- alpha_n(
		train = train,
		valid = valid,
		vars = c("a", "w"),
		architecture = architecture,
		.f = .f1,
		weights = NULL,
		control = control
		)

	alpha2 <- alpha_n(
		train = train,
		valid = valid,
		vars = c("a", "z", "w"),
		architecture = architecture,
		.f = .f2,
		weights = alpha1,
		control = control
	)

	alpha3 <- alpha_n(
		train = train,
		valid = valid,
		vars = c("a", "m", "z", "w"),
		architecture = architecture,
		.f = .f3,
		weights = alpha2,
		control = control
	)

	list(jkl = gsub("data_", "", paste0(j, k, l, collapse = "")),
			 alpha1 = alpha1,
			 alpha2 = alpha2,
			 alpha3 = alpha3)
}
