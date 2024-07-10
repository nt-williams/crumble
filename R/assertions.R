check_for_missing <- function(data, A, W, M, Z, C) {
	check <- data[, c(A, W, M, Z, C), drop = FALSE]

	if (any(is.na(check))) {
		return("Missing data found in treatment/covariate/mediator/observed nodes")
	}

	TRUE
}

assert_not_missing <- checkmate::makeAssertionFunction(check_for_missing)

check_binary_0_1 <- function(x) {
	# Check if there are exactly two unique values and they are not 0 and 1
	if (length(x) == 2 && !all(x %in% c(0, 1))) {
		return("The outcome contains exactly two unique values, but they are not 0 and 1")
	}

	TRUE
}

assert_binary_0_1 <- checkmate::makeAssertionFunction(check_binary_0_1)

check_effect_type <- function(moc, effect) {
	if (is.null(moc) & effect == "RT") {
		return("Must provide mediator-outcome confounders for recanting twins")
	}

	if (is.null(moc) & effect == "RI") {
		return("Must provide mediator-outcome confounders for interventional effects")
	}

	if (!is.null(moc) & effect %in% c("N", "O", "D")) {
		return("Must not provide mediator-outcome confounders for natural, organic, or decision theoretic effects")
	}

	TRUE
}

assert_effect_type <- checkmate::makeAssertionFunction(check_effect_type)
