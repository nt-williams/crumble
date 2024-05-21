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
