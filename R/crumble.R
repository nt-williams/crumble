crumble <- function(data, trt, outcome, mediators, moc, covar, cens = NA_character_,
										control = crumble_control()) {
	assert_not_data_table(data)

	vars <- crumble_vars(
		A = trt,
		Y = outcome,
		M = mediators,
		Z = moc,
		W = covar,
		C = cens
	)

	tmp <- convert_NA(data, vars)                          # convert NA Y to -999
	folds <- make_folds(tmp, control@crossfit_folds)       # create folds for cross fitting

	propensity  <- g(data, vars, folds, control@learners_propensity)
	propensity2 <- e(data, vars, folds, control@learners_propensity2) # Need to come up with a better name
	outcome_reg <- m(data, vars, folds, control@learners_outcome)


}
