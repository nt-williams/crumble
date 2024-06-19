#' Mediation Analysis Using Modified Treatment Policies and Recanting Twins
#'
#' @param data [\code{data.frame}]\cr
#'  A \code{data.frame} in wide format containing all necessary variables
#'  for the estimation problem.
#' @param trt [\code{character}]\cr
#'  A vector containing the column names of treatment variables.
#' @param outcome [\code{character(1)}]\cr
#'  The column name of the outcome variable.
#' @param mediators
#' @param moc
#' @param covar [\code{character}]\cr
#'  An optional vector containing the column names of baseline covariates to be
#'  controlled for.
#' @param cens [\code{character(1)}]\cr
#'  An optional vector of column name of a censoring indicator. Must be provided if
#'  there is missingness in the outcome.
#' @param id [\code{character(1)}]\cr
#'  An optional column name containing cluster level identifiers.
#' @param d0 [\code{closure}]\cr
#'  A two argument function that specifies how treatment variables should be shifted.
#'  See examples for how to specify shift functions for continuous, binary, and categorical exposures.
#' @param d1 [\code{closure}\]\cr
#'  A two argument function that specifies how treatment variables should be shifted.
#'  See examples for how to specify shift functions for continuous, binary, and categorical exposures.
#' @param learners_regressions [\code{character}]\cr A vector of \code{mlr3superlearner} algorithms
#'  for estimation of the outcome regressions. Default is \code{"glm"}, a main effects GLM.
#' @param nn_riesz_module
#' @param control
#'
#' @return
#' @export
#'
#' @examples
crumble <- function(data,
										trt,
										outcome,
										mediators,
										moc = NULL,
										covar,
										cens = NULL,
										id = NULL,
										d0 = NULL,
										d1 = NULL,
										learners_regressions = "glm",
										nn_riesz_module = nn_sequential_architecture,
										control = crumble_control()) {

	# Perform initial checks
	checkmate::assert_data_frame(data[, c(trt, outcome, mediators, moc, covar, cens, id)])
	assert_not_missing(data, trt, covar, mediators, moc)
	checkmate::assert_function(d0, nargs = 2, null.ok = TRUE)
	checkmate::assert_function(d1, nargs = 2, null.ok = TRUE)
	checkmate::assert_function(nn_riesz_module)

	call <- match.call()

	# Create crumble_data object
	cd <- crumble_data(
		data = data,
		vars = crumble_vars(
			A = trt,
			Y = outcome,
			M = mediators,
			Z = moc %??% NA_character_,
			W = covar,
			C = cens %??% NA_character_,
			id = id %??% NA_character_
		),
		d0 = d0,
		d1 = d1
	)

	# Create permuted Z
	if (!is.null(moc)) {
		cd@data_0zp <- cd@data_0
		cd@data_0zp[, cd@vars@Z] <- set_zp(cd)
	}

	# Create folds for cross fitting
	folds <- make_folds(cd@data, control@crossfit_folds)

	# Estimate \theta nuisance parameters
	thetas <- foreach::foreach(i = 1:control@crossfit_folds,
														 # .combine = \(...) recombine_theta(..., folds = folds),
														 .options.future = list(seed = TRUE)) %dofuture% {
		# Training
		train <- training(cd, folds, i)
		# Validation
		valid <- validation(cd, folds, i)

		theta(train, valid, cd@vars, learners_regressions, control)
	}

	thetas <- recombine_theta(thetas, folds)

	alpha_ns <- foreach::foreach(i = 1:control@crossfit_folds) %dofuture% {
		# Training
		train <- training(cd, folds, i)
		# Validation
		valid <- validation(cd, folds, i)

		if (!is.null(moc)) {
			list(
				"000" = phi_n_alpha(train, valid, cd@vars, nn_riesz_module, "data_0", "data_0", "data_0", control),
				"111" = phi_n_alpha(train, valid, cd@vars, nn_riesz_module, "data_1", "data_1", "data_1", control),
				"011" = phi_n_alpha(train, valid, cd@vars, nn_riesz_module, "data_0", "data_1", "data_1", control),
				"010" = phi_n_alpha(train, valid, cd@vars, nn_riesz_module, "data_0", "data_1", "data_0", control)
			)
		} else {
			# The values of l are arbitrary in this case
			list(
				"00" = phi_n_alpha(train, valid, cd@vars, nn_riesz_module, "data_0", "data_0", "data_0", control),
				"11" = phi_n_alpha(train, valid, cd@vars, nn_riesz_module, "data_1", "data_1", "data_1", control),
				"01" = phi_n_alpha(train, valid, cd@vars, nn_riesz_module, "data_0", "data_1", "data_1", control)
			)
		}
	}

	alpha_ns <- recombine_alpha(alpha_ns, folds)

	if (!is.null(moc)) {
		alpha_rs <- foreach::foreach(i = 1:control@crossfit_folds) %dofuture% {
			# Training
			train <- training(cd, folds, i)
			# Validation
			valid <- validation(cd, folds, i)

			list(
				"0111" = phi_r_alpha(train, valid, cd@vars, nn_riesz_module, "data_0zp", "data_1", "data_1", "data_1", control),
				"0011" = phi_r_alpha(train, valid, cd@vars, nn_riesz_module, "data_0zp", "data_0", "data_1", "data_1", control),
				"0010" = phi_r_alpha(train, valid, cd@vars, nn_riesz_module, "data_0zp", "data_0", "data_1", "data_0", control)
			)
		}

		alpha_rs <- recombine_alpha(alpha_rs, folds)

		eif_ns <- sapply(c("000", "111", "011", "010"), \(jkl) eif_n(cd, thetas$theta_n, alpha_ns, jkl))
		eif_rs <- sapply(c("0111", "0011", "0010"), \(ijkl) eif_r(cd, thetas$theta_r, alpha_rs, ijkl))
	} else {
		eif_ns <- sapply(c("00", "11", "01"), \(jk) eif_natural(cd, thetas$theta_n, alpha_ns, jk))
		eif_rs <- NULL
		alpha_rs <- NULL
	}

	# Estimates ---------------------------------------------------------------

	out <- list(
		estimates = calc_estimates(eif_ns, eif_rs),
		outcome_reg = thetas,
		alpha_n = alpha_ns,
		alpha_r = alpha_rs,
		fits = list(theta_n = thetas$theta_n$weights,
								theta_r = thetas$theta_r$weights),
		call = call,
		natural = is.null(moc)
	)

	class(out) <- "crumble"
	out
}
