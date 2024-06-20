alpha <- function(model, data) {
	model(data)[, 1]
}

add_psuedo <- function(data, x) {
	cbind("tmp_crumble_pseudo_y" = x, data)
}

calc_stderror <- function(eif) {
	sqrt(var(eif) / length(eif))
}

calc_ci <- function(x, eif) {
	se <- calc_stderror(eif)
	x + c(-1, 1)*se*qnorm(0.975)
}

dist2 <- function(x, y) {
	X_sq <- rowSums(x^2)
	Y_sq <- rowSums(y^2)
	outer(X_sq, Y_sq, "+") - 2 * tcrossprod(x, y)
}

rbf <- function(sigma) {
	constant <- 1 / (2*(sigma^2))
	\(x, y) exp(-constant*dist2(x, y))
}

rkhs_distance <- function(Z, kernel) {
	# Compute the kernel matrix
	K <- kernel(Z, Z)
	# Create a column vector of ones
	O <- matrix(1, nrow = nrow(Z), ncol = 1)
	# Compute the N matrix
	N <- O %*% diag(K)
	# Compute the pairwise RKHS distances
	sqrt(N + t(N) - 2 * K)
}

create_doubly_stochastic <- function(n) {
	# Create an identity matrix
	identity_mat <- diag(1, n)
	identity_mat[sample(n), sample(n)]
}

shift_data <- function(data, trt, cens, shift) {
	if (is.null(shift)) {
		return(shift_cens(data, cens))
	}
	shift_trt(shift_cens(data, cens), trt, shift)
}

shift_cens <- function(data, cens) {
	if (is.na(cens)) {
		return(data)
	}

	out <- as.list(data)
	for (ce in cens) {
		out[[ce]] <- 1
	}
	as.data.frame(out, check.names = FALSE)
}

shift_trt <- function(data, trt, .f) {
	for (a in trt) {
		data[[a]] <- .f(data, a)
	}
	data
}

make_folds <- function(data, V) {
	folds <- origami::make_folds(data, V = V)
	if (V == 1) {
		folds[[1]]$training_set <- folds[[1]]$validation_set
	}
	folds
}

as_torch <- function(data) {
	torch::torch_tensor(as.matrix(data), dtype = torch::torch_float())
}

censored <- function(data, cens) {
	# when no censoring return TRUE for all obs
	if (is.na(cens)) {
		return(rep(TRUE, nrow(data)))
	}

	# other wise find censored observations
	return(data[[cens]] == 1)
}

# Function to check if a variable only contains 0, 1, and NA values
is_binary <- function(x) {
	# Remove NA values
	non_na_values <- na.omit(x)

	# Get unique values quickly using unique()
	unique_values <- unique(non_na_values)

	assert_binary_0_1(unique_values)

	# Check if all non-NA values are either 0 or 1
	if (!all(non_na_values %in% c(0, 1))) {
		return(FALSE)
	}

	TRUE
}

reduce_bind_reorder <- function(x, .f, name1, name2, i) {
	vals <- lapply(x, \(y) y[[name1]]) |>
		lapply(\(z) z[[name2]]) |>
		Reduce(.f, x = _)
	vals[order(i)]
}

recombine_theta <- function(x, folds) {
	# x <- list(...)
	ind <- Reduce(c, lapply(folds, function(x) x[["validation_set"]]))
	.f <- function(x) {
		ijkl <- names(x[[2]])

		b_names <- grep("^b", names(x[[2]][[1]]), value = TRUE)
		bs <- lapply(b_names, \(param) sapply(ijkl, \(ijkl) reduce_bind_reorder(x, c, ijkl, param, ind)))
		names(bs) <- b_names

		natural_names <- grep("_natural$", names(x[[2]][[1]]), value = TRUE)
		natural <- lapply(natural_names, \(param) sapply(ijkl, \(ijkl) reduce_bind_reorder(x, c, ijkl, param, ind)))
		names(natural) <- natural_names

		weight_names <- grep("_weights$", names(x[[2]][[1]]), value = TRUE)
		weights <- setNames(
			lapply(weight_names, function(fit) {
				setNames(lapply(ijkl, \(ijkl) lapply(lapply(x, \(y) y[[ijkl]]), \(z) z[[fit]])),
								 ijkl)
			}),
			weight_names
		)

		list(bs = bs,
				 natural = natural,
				 weights = weights)
	}

	list(theta_n = .f(lapply(x, \(x) x$n)),
			 theta_r = .f(lapply(x, \(x) x$r)))
}

recombine_alpha <- function(x, folds) {
	# x <- list(...)
	ind <- Reduce(c, lapply(folds, function(x) x[["validation_set"]]))
	ijkl <- names(x[[2]])
	alpha_names <- grep("^alpha", names(x[[2]][[1]]), value = TRUE)
	setNames(
		lapply(alpha_names,
					 \(param) sapply(ijkl,
					 								\(ijkl) reduce_bind_reorder(x, c, ijkl, param, ind))),
		alpha_names
	)
}

calc_estimates <- function(eif_ns, eif_rs = NULL) {
	if (is.null(eif_rs)) {
		return(calc_estimates_natural(eif_ns))
	} else {
		return(calc_estimates_rt(eif_ns, eif_rs))
	}
}

calc_estimates_natural <- function(eif_ns) {
	list(
		# A -> Y
		p1 = mean(eif_ns[, "11"] - eif_ns[, "01"]),
		eif_p1 = eif_ns[, "11"] - eif_ns[, "01"],
		# A -> M -> Y
		p4 = mean(eif_ns[, "01"] - eif_ns[, "00"]),
		eif_p4 = eif_ns[, "01"] - eif_ns[, "00"],
		ate = mean(eif_ns[, "11"] - eif_ns[, "00"]),
		eif_ate = eif_ns[, "11"] - eif_ns[, "00"]
	)
}

calc_estimates_rt <- function(eif_ns, eif_rs) {
	list(
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
}

no_Z <- function(vars) any(is.na(vars@Z))
