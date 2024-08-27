#' Mediation analysis using modified treatment policies and recanting twins
#'
#' @param data [\code{data.frame}]\cr
#'  A \code{data.frame} in wide format containing all necessary variables
#'  for the estimation problem.
#' @param trt [\code{character}]\cr
#'  A vector containing the column names of treatment variables.
#' @param outcome [\code{character(1)}]\cr
#'  The column name of the outcome variable.
#' @param mediators [\code{character}]\cr
#'	A vector containing the column names of the mediator variables.
#' @param moc [\code{character}]\cr
#'  An optional vector containing the column names of the mediator-outcome confounders.
#' @param covar [\code{character}]\cr
#'  An vector containing the column names of baseline covariates to be
#'  controlled for.
#' @param obs [\code{character(1)}]\cr
#'  An optional column name (with values coded as 0 or 1) for whether or not the \code{outcome} is observed.
#'  Must be provided if there is missingness in the outcome! Default is \code{NULL}.
#' @param id [\code{character(1)}]\cr
#'  An optional column name containing cluster level identifiers.
#' @param d0 [\code{closure}]\cr
#'  A two argument function that specifies how treatment variables should be shifted.
#'  See examples for how to specify shift functions for continuous, binary, and categorical exposures.
#' @param d1 [\code{closure}]\cr
#'  A two argument function that specifies how treatment variables should be shifted.
#'  See examples for how to specify shift functions for continuous, binary, and categorical exposures.
#' @param effect [\code{character(1)}]\cr
#'  The type of effect to estimate. Options are \code{"RT"} for recanting twins,
#'  \code{"N"} for natural effects, \code{"RI"} for randomized interventional effects,
#'  and \code{"O"} for organic effects.
#'  If \code{"RT"} or \code{"RI"} is selected, \code{moc} must be provided.
#'  If \code{"N"} or \code{"O"} is selected, \code{moc} must be \code{NULL}.
#' @param learners [\code{character}]\cr
#'  A vector of \code{mlr3superlearner} algorithms
#'  for estimation of the outcome regressions. Default is \code{"glm"}, a main effects GLM.
#' @param nn_module [\code{function}]\cr A function that returns a neural network module.
#' @param control [\code{crumble_control}]\cr
#'  Control parameters for the estimation procedure. Use \code{crumble_control()} to set these values.
#'
#' @return A \code{crumble} object containing the following components:
#' \item{estimates}{A list of parameter estimates.}
#' \item{outcome_reg}{Predictions from the outcome regressions.}
#' \item{alpha_n}{A list of density ratio estimates.}
#' \item{alpha_r}{A list of density ratio estimates.}
#' \item{fits}{A list of the fitted values from the outcome regressions.}
#' \item{call}{The matched call.}
#' \item{effect}{The estimated effect type.}
#'
#' @export
#'
#' @example inst/examples/examples.R
crumble <- function(data,
										trt,
										outcome,
										mediators,
										moc = NULL,
										covar,
										obs = NULL,
										id = NULL,
										d0 = NULL,
										d1 = NULL,
										effect = c("RT", "N", "RI", "O"),
										weights = rep(1, nrow(data)),
										learners_regressions = "glm",
										nn_module = sequential_module(),
										control = crumble_control()) {
	# Perform initial checks
	checkmate::assert_data_frame(data[, c(trt, outcome, mediators, moc, covar, obs, id)])
	assert_not_missing(data, trt, covar, mediators, moc, obs)
	checkmate::assert_function(d0, nargs = 2, null.ok = TRUE)
	checkmate::assert_function(d1, nargs = 2, null.ok = TRUE)
	checkmate::assert_function(nn_module)
	if (!is.null(obs)) assert_binary_0_1(data[[obs]])
	assert_effect_type(moc, match.arg(effect))
	checkmate::assertNumeric(weights, len = nrow(data), finite = TRUE, any.missing = FALSE)

	weights <- normalize(weights)

	params <- switch(match.arg(effect),
									 N = natural,
									 O = organic,
									 D = decision,
									 RT = recanting_twin,
									 RI = randomized)

	# Create crumble_data object
	cd <- crumble_data(
		data = data,
		vars = crumble_vars(
			A = trt,
			Y = outcome,
			M = mediators,
			Z = moc %??% NA_character_,
			W = covar,
			C = obs %??% NA_character_,
			id = id %??% NA_character_
		),
		d0 = d0,
		d1 = d1
	)

	# Create permuted Z
	cd <- add_zp(cd, moc, control)

	# Create folds for cross fitting
	folds <- make_folds(cd@data, control$crossfit_folds, cd@vars@id, cd@vars@Y)

	# Estimate \theta nuisance parameters
	thetas <- estimate_theta(cd, thetas, folds, params, learners_regressions, control)

	# Estimate density ratios, alpha natural
	alpha_ns <- estimate_phi_n_alpha(cd, folds, params, nn_module, control)
	if (!is.null(alpha_ns)) {
		eif_ns <- sapply(colnames(alpha_ns[[1]]), \(jkl) eif_n(cd, thetas$theta_n, alpha_ns, jkl))
	} else {
		eif_ns <- NULL
	}

  # Estimate density ratios, alpha randomized
	alpha_rs <- estimate_phi_r_alpha(cd, folds, params, nn_module, control)
	if (!is.null(alpha_rs)) {
		eif_rs <- sapply(colnames(alpha_rs[[1]]), \(ijkl) eif_r(cd, thetas$theta_r, alpha_rs, ijkl))
	} else {
		eif_rs <- NULL
	}

	# Estimates ---------------------------------------------------------------

	out <- list(
		estimates = switch(match.arg(effect),
											 N = calc_estimates_natural(eif_ns, weights),
											 O = calc_estimates_organic(eif_ns, weights),
											 RT = calc_estimates_rt(eif_ns, eif_rs, weights),
											 RI = calc_estimates_ri(eif_rs, weights)),
		outcome_reg = thetas,
		alpha_n = alpha_ns,
		alpha_r = alpha_rs,
		fits = list(theta_n = thetas$theta_n$weights,
								theta_r = thetas$theta_r$weights),
		call = match.call(),
		effect = match.arg(effect),
		id = cd@data[[cd@vars@id]],
		weights = weights
	)

	class(out) <- "crumble"
	out
}
