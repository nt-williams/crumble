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
	Reduce(.f, lapply(lapply(x, \(y) y[[name1]]), \(z) z[[name2]]))[order(i)]
}

recombine_theta_n <- function(..., folds) {
	x <- list(...)
	ind <- Reduce(c, lapply(folds, function(x) x[["validation_set"]]))

	bs <- lapply(paste0("b", 1:3),
							 \(param) sapply(c("000", "111", "011", "010"),
							 								\(jkl) reduce_bind_reorder(x, c, jkl, param, ind)))
	names(bs) <- paste0("b", 1:3)

	natural <- lapply(paste0(paste0("fit", 1:3), "_natural"),
										\(param) sapply(c("000", "111", "011", "010"),
																		\(jkl) reduce_bind_reorder(x, c, jkl, param, ind)))
	names(natural) <- paste0("natural", 1:3)

	weights <- setNames(
		lapply(paste0(paste0("fit", 1:3), "_weights"), function(fit) {
			setNames(lapply(c("000", "111", "011", "010"),
											\(jkl) lapply(lapply(x, \(y) y[[jkl]]), \(z) z[[fit]])),
							 c("000", "111", "011", "010"))
		}),
		paste0("weights", 1:3)
	)

	list(bs = bs,
			 natural = natural,
			 weights = weights)
}

recombine_alpha_n <- function(..., folds) {
	x <- list(...)
	ind <- Reduce(c, lapply(folds, function(x) x[["validation_set"]]))

	setNames(
		lapply(paste0("alpha", 1:3),
					 \(param) sapply(c("000", "111", "011", "010"),
					 								\(jkl) reduce_bind_reorder(x, c, jkl, param, ind))),
		paste0("alpha", 1:3)
	)
}
