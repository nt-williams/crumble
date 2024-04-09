crossfit <- function(train, valid, y, continuous = FALSE, id = NULL, learners, bound = FALSE) {
	fit <- mlr3superlearner(
		data = train,
		target = y,
		library = learners,
		outcome_type = ifelse(continuous, "continuous", "binomial"),
		folds = NULL,
		newdata = valid,
		group = id
	)
	preds <- lapply(fit$predseds, function(x) bound(x))
	list(weights = fit$weights, preds = preds)
}

bound <- function(x, p = 1e-03) {
	pmax(pmin(x, 1 - p), p)
}
