eif_n <- function(cd, theta, alpha, jkl) {
	Y <- ifelse(is.na(cd@data[[cd@vars@Y]]), -999, cd@data[[cd@vars@Y]])
	mult <- ifelse(is.na(cd@data[[cd@vars@Y]]), 0, 1)

	alpha$alpha3[, jkl]*mult*(Y - theta$theta_n$natural$fit3_natural[, jkl]) +
		alpha$alpha2[, jkl]*(theta$theta_n$bs$b3[, jkl] - theta$theta_n$natural$fit2_natural[, jkl]) +
		alpha$alpha1[, jkl]*(theta$theta_n$bs$b2[, jkl] - theta$theta_n$natural$fit1_natural[, jkl]) +
		theta$theta_n$bs$b1[, jkl]
}

eif_r <- function(cd, theta, alpha, ijkl) {
	Y <- ifelse(is.na(cd@data[[cd@vars@Y]]), -999, cd@data[[cd@vars@Y]])
	mult <- ifelse(is.na(cd@data[[cd@vars@Y]]), 0, 1)

	alpha$alpha4[, ijkl]*mult*(Y - theta$theta_r$natural$fit4_natural[, ijkl]) +
		alpha$alpha3[, ijkl]*(theta$theta_r$bs$b4[, ijkl] - theta$theta_r$natural$fit3_natural[, ijkl]) +
		alpha$alpha2[, ijkl]*(theta$theta_r$bs$b3[, ijkl] - theta$theta_r$natural$fit2_natural[, ijkl]) +
		alpha$alpha1[, ijkl]*(theta$theta_r$bs$b2[, ijkl] - theta$theta_r$natural$fit1_natural[, ijkl]) +
		theta$theta_r$bs$b1[, ijkl]
}

calc_eifs <- function(cd, alphas, thetas, .f) {
	if (is.null(alphas)) return(NULL)
	eifs <- sapply(colnames(alphas[[1]]), \(x) .f(cd, thetas, alphas, x))
	id <- cd@data[[cd@vars@id]]
	apply(eifs, 2, \(j) {
		ife::ife(
			x = weighted.mean(j, cd@weights),
			eif = j,
			weights = cd@weights,
			id = as.character(cd@data[[cd@vars@id]] %??% rep(1, nrow(eifs)))
		)
	})
}
