linear_permutation <- function(data) {
	D <- dist(data)
	D <- D / max(D)
	d  <- as.vector(t(as.matrix(D)))
	n <- nrow(data)
	rows <- c(as.numeric(gl(n, n, n^2)), as.numeric(gl(n, n, n^2)) + n)
	cols <- c(1:(n^2), unlist(lapply(0:(n - 2), function(j) j + seq(1, n^2, n))), (0:(n - 1)*(n + 1) + 1))
	A <- Matrix::sparseMatrix(i = rows, j = cols, x = 1)
	b <- Matrix::Matrix(Matrix::sparseVector(i = 1:(2*n - 1), x = 1, length = 2*n), ncol = 1)
	matrix(Rsymphony::Rsymphony_solve_LP(d, A, dir = rep("==", nrow(b)), rhs = b)$solution, n, n)
}

set_zp <- function(cd, folds) {
	folds <- make_folds(cd@data, folds)

	AW <- one_hot_encode(cd@data, c(cd@vars@A, cd@vars@W))
	Z <- cd@data[, cd@vars@Z, drop = FALSE]

	permute <- function(i) {
		zp <- data.frame(matrix(NA, nrow = nrow(AW), ncol = ncol(Z)))
		names(zp) <- names(Z)
		P <- linear_permutation(AW[i, ])
		for (z in names(zp)) {
			zp[i, z] <- as.vector(P %*% Z[i, z])
		}
		zp
	}

	permuted <- vector("list", length(folds))
	i <- 1
	cli::cli_progress_step("Permuting Z-prime variables... {i}/{length(folds)} tasks")
	for (i in seq_along(folds)) {
		permuted[[i]] <- permute(folds[[i]]$validation_set) |>
			as.list()
		cli::cli_progress_update()
	}

	cli::cli_progress_done()
	revert_list(permuted) |>
		purrr::map(\(x) purrr::reduce(x, data.table::fcoalesce)) |>
		data.frame()
}

# https://stackoverflow.com/questions/15263146/revert-list-structure
revert_list <- function(ls) { # @Josh O'Brien
	# get sub-elements in same order
	x <- lapply(ls, `[`, names(ls[[1]]))
	# stack and reslice
	apply(do.call(rbind, x), 2, as.list)
}
