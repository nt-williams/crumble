eif_n <- function(cd, theta, alpha, jkl) {
	Y <- cd@data[[cd@vars@Y]]

	alpha$alpha3[, jkl]*(Y - theta$natural$natural3[, jkl]) +
		alpha$alpha2[, jkl]*(theta$bs$b3[, jkl] - theta$natural$natural2[, jkl]) +
		alpha$alpha1[, jkl]*(theta$bs$b2[, jkl] - theta$natural$natural1[, jkl]) +
		theta$bs$b1[, jkl]
}
