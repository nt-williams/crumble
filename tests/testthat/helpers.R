gendata <- function(n = 1e3) {
	w <- rbinom(n, 1, 0.25)
	a <- rbinom(n, 1, 0.5)
	z <- rbinom(n, 1, plogis(0.25*w + 0.75*a))
	m <- rbinom(n, 1, plogis(0.125*w + 0.5*a - 0.25*z))
	y <- rbinom(n, 1, plogis(-1 + 0.25*w + 0.75*a + 0.25*z + 0.25*m))
	data.frame(
		w = w,
		a = a,
		z = z,
		m = m,
		y = y
	)
}

datagen2 <- function(n) {
	lambda1 = 0.8
	lambda2 = 0.6
	gamma1 = 0.6
	gamma2 = 0.4

	W_1 <- rbeta(n, 2, 3)
	W_2 <- rbeta(n, 2, 3)
	W_3 <- rbeta(n, 2, 3)
	A <- rbinom(n, 1, plogis(0.5 * W_1 + 0.5 * W_2 - 1))
	Z_1 <- truncnorm::rtruncnorm(n, a = -1, b = 1, mean = -0.4 + 0 * A + 0.2 * (W_3) ** 2)
	Z_2 <- truncnorm::rtruncnorm(n, a = -1, b = 1, mean = 0.2 - 0 * A + 0.5 * sin(W_2))
	M_1 <- truncnorm::rtruncnorm(n, a = -1, b = 1, mean = -0.5 + lambda1 * Z_1 + lambda2 * A + 0.4 * W_2 + 0.2 * W_3)
	M_2 <- truncnorm::rtruncnorm(n, a = -1, b = 1, mean = -0.5 + lambda1 * Z_2 + lambda2 * A + 0.4 * W_1 + 0.2 * W_3)
	Y <- rnorm(n, mean = 0.2 * M_1 + 0.2 * M_2 + gamma1 * Z_1 / 2 + gamma1 * Z_2 / 2 + gamma2 * A - 0.5 * cos(W_1) - 1.5)
	data <- data.frame(W_1, W_2, W_3, A, Z_1, Z_2, M_1, M_2, Y)
	return(data)
}
