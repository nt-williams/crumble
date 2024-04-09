m <- function(data, vars, folds, learners) {
	preds <- matrix(nrow = nrow(data), ncol = 2)
	colnames(preds) <- c("m(0,Z,M,W)", "m(1,Z,M,W)")
	fits <- vector("list", length(folds))
	is_continuous <- schoolmath::is.decimal(data[[vars@Y]])

	for (fold in seq_along(folds)) {
		train <- origami::training(data, folds[[fold]])
		valid <- lapply(c(0, 1), function(value) set_trt(origami::validation(data, folds[[fold]]), vars, value))

		if (!is.na(vars$C)) {
			obs <- train[, vars$C] == 0
		} else {
			obs <- rep(TRUE, nrow(train))
		}

		nuis <- crossfit(
			train[obs, c(vars@Y, vars@W, vars@A, vars@Z, vars@M)],
			valid,
			vars@Y,
			is_continuous,
			learners = learners
		)

		fits[[fold]] <- nuis$weights
		preds[folds[[fold]]$validation_set, "m(0,Z,M,W)"] <- nuis$preds[[1]]
		preds[folds[[fold]]$validation_set, "m(1,Z,M,W)"] <- nuis$preds[[2]]
	}

	list(preds = preds, fits = fits)
}
