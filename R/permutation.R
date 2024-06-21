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

	cli::cli_progress_step("Permuting Z-prime variables...")
	permute <- function(cd, i) {
		zp <- data.frame(matrix(NA, nrow = nrow(cd@data), ncol = length(cd@vars@Z)))
		names(zp) <- cd@vars@Z
		P <- linear_permutation(cd@data[i, c(cd@vars@A, cd@vars@W)])
		for (z in cd@vars@Z) {
			zp[i, z] <- as.vector(P %*% cd@data[i, z])
		}
		zp
	}

	purrr::map(folds, \(x) permute(cd, x$validation_set)) |>
		purrr::map(as.list) |>
		revert_list() |>
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
