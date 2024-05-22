eif_n <- function(cd, theta, alpha, jkl) {
	Y <- cd@data[[cd@vars@Y]]

	alpha$alpha3[, jkl]*(Y - theta$natural$fit3_natural[, jkl]) +
		alpha$alpha2[, jkl]*(theta$bs$b3[, jkl] - theta$natural$fit2_natural[, jkl]) +
		alpha$alpha1[, jkl]*(theta$bs$b2[, jkl] - theta$natural$fit1_natural[, jkl]) +
		theta$bs$b1[, jkl]
}

eif_r <- function(cd, theta, alpha, ijkl) {
	Y <- cd@data[[cd@vars@Y]]

	alpha$alpha4[, ijkl]*(Y - theta$natural$fit4_natural[, ijkl]) +
		alpha$alpha3[, ijkl]*(theta$bs$b4[, ijkl] - theta$natural$fit3_natural[, ijkl]) +
		alpha$alpha2[, ijkl]*(theta$bs$b3[, ijkl] - theta$natural$fit2_natural[, ijkl]) +
		alpha$alpha1[, ijkl]*(theta$bs$b2[, ijkl] - theta$natural$fit1_natural[, ijkl]) +
		theta$bs$b1[, ijkl]
}
