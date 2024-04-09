g <- function(data, vars, folds, learners) {
	preds <- matrix(nrow = nrow(data), ncol = 2)
	colnames(preds) <- c("g(0|w)", "g(1|w)")
	fits <- vector("list", length(folds))

	for (fold in seq_along(folds)) {
		train <- origami::training(data, folds[[fold]])
		valid <- origami::validation(data, folds[[fold]])

		nuis <- crossfit(
			train[, c(vars@A, vars@W)],
			list(valid),
			vars@A,
			FALSE,
			learners = learners,
			bound = TRUE
		)

		fits[[fold]] <- nuis$weights

		preds[folds[[v]]$validation_set, "g(0|w)"] <- 1 - nuis$preds
		preds[folds[[v]]$validation_set, "g(1|w)"] <- nuis$preds
	}
	list(preds = preds, fits = fits)
}
