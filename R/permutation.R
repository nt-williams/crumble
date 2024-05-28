linear_permutation <- function(data) {
	D <- dist(data)
	D <- D / max(D)
	d  <- as.vector(t(as.matrix(D)))
	n <- nrow(data)
	rows <- c(as.numeric(gl(n, n, n^2)), as.numeric(gl(n, n, n^2)) + n)
	cols <- c(1:(n^2), unlist(lapply(0:(n - 2), function(j) j + seq(1, n^2, n))), (0:(n - 1)*(n + 1) + 1))
	A <- Matrix::sparseMatrix(i = rows, j = cols, x = 1)
	b <- Matrix::Matrix(Matrix::sparseVector(i = 1:(2*n - 1), x = 1, length = 2*n), ncol = 1)
	# matrix(Rglpk::Rglpk_solve_LP(d, A, dir = rep("==", nrow(b)), rhs = b)$solution, n, n)
	matrix(Rsymphony::Rsymphony_solve_LP(d, A, dir = rep("==", nrow(b)), rhs = b)$solution, n, n)
}

set_zp <- function(cd) {
	P <- linear_permutation(cd@data[, c(cd@vars@A, cd@vars@W)])
	zp <- cd@data[, cd@vars@Z, drop = FALSE]
	for (z in cd@vars@Z) {
		zp[, z] <- as.vector(P %*% cd@data[, z])
	}
	zp
}
