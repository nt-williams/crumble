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
#' @param effect [\code{character}]\cr
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
#' \item{natural}{A logical indicating if the natural direct effect is being estimated.}
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
										effect = c("RT", "N", "RI", "O", "D"),
										learners_regressions = "glm",
										nn_module = sequential_module(),
										control = crumble_control()) {

	# check for character vector and error out that they need to be made into factors

	# Perform initial checks
	checkmate::assert_data_frame(data[, c(trt, outcome, mediators, moc, covar, obs, id)])
	assert_not_missing(data, trt, covar, mediators, moc, obs)
	checkmate::assert_function(d0, nargs = 2, null.ok = TRUE)
	checkmate::assert_function(d1, nargs = 2, null.ok = TRUE)
	checkmate::assert_function(nn_module)
	if (!is.null(obs)) assert_binary_0_1(data[[obs]])
	assert_effect_type(moc, effect)

	call <- match.call()

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
	if (!is.null(moc)) {
		cd@data_0zp <- cd@data_0
		cd@data_0zp[, cd@vars@Z] <- set_zp(cd, control$zprime_folds)
	}

	# Create folds for cross fitting
	folds <- make_folds(cd@data, control$crossfit_folds)
	thetas <- alpha_rs <- alpha_ns <- vector("list", control$crossfit_folds)

	# Estimate \theta nuisance parameters
	i <- 1
	cli::cli_progress_step("Fitting outcome regressions... {i}/{control$crossfit_folds} folds")
	for (i in seq_along(thetas)) {
		# Training
		train <- training(cd, folds, i)
		# Validation
		valid <- validation(cd, folds, i)

		thetas[[i]] <- theta(train, valid, cd@vars, params, learners_regressions, control)
		cli::cli_progress_update()
	}

	cli::cli_progress_done()
	thetas <- recombine_theta(thetas, folds)

	i <- 1
	cli::cli_progress_step("Computing alpha n density ratios... {i}/{control$crossfit_folds} folds")
	for (i in seq_along(folds)) {
		# Training
		train <- training(cd, folds, i)
		# Validation
		valid <- validation(cd, folds, i)

		alpha_ns[[i]] <- lapply(
			params$natural,
			\(param) phi_n_alpha(train, valid, cd@vars, nn_module, param, control)
		)

		names(alpha_ns[[i]]) <- unlist(lapply(params$natural, \(x) paste0(gsub("data_", "", x), collapse = "")))

		cli::cli_progress_update()
	}

	cli::cli_progress_done()
	alpha_ns <- recombine_alpha(alpha_ns, folds)

	if (length(params$randomized) != 0) {
		i <- 1
		cli::cli_progress_step("Computing alpha r density ratios... {i}/{control$crossfit_folds} folds")
		for (i in seq_along(folds)) {
			# Training
			train <- training(cd, folds, i)
			# Validation
			valid <- validation(cd, folds, i)

			alpha_rs[[i]] <- lapply(
				params$randomized,
				\(param) phi_r_alpha(train, valid, cd@vars, nn_module, param, control)
			)

			names(alpha_rs[[i]]) <-
				gsub("zp", "", unlist(lapply(params$randomized, \(x) paste0(gsub("data_", "", x), collapse = ""))))

			cli::cli_progress_update()
		}

		alpha_rs <- recombine_alpha(alpha_rs, folds)
		eif_rs <- sapply(colnames(alpha_rs[[1]]), \(ijkl) eif_r(cd, thetas$theta_r, alpha_rs, ijkl))
	} else {
		alpha_rs <- NULL
		eif_rs <- NULL
	}

	eif_ns <- sapply(colnames(alpha_ns[[1]]), \(jkl) eif_n(cd, thetas$theta_n, alpha_ns, jkl))

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
