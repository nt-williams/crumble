crossfit <- function(train, valid, y, continuous = FALSE,
										 id = NULL, learners, folds = NULL, bound = FALSE) {
	fit <- mlr3superlearner::mlr3superlearner(
		data = train,
		target = y,
		library = learners,
		outcome_type = ifelse(continuous, "continuous", "binomial"),
		folds = folds,
		newdata = valid,
		group = id
	)

	if (bound) {
		preds <- lapply(fit$preds, function(x) bound(x))
	} else {
		preds <- fit$preds
	}

	list(weights = fit$weights, preds = preds)
}

bound <- function(x, p = 1e-03) {
	pmax(pmin(x, 1 - p), p)
}
