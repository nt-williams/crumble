theta_n <- function(train, valid, vars, learners, control) {
	continuous <- !is_binary(train$data[[vars@Y]])

	theta3 <- crossfit(
		train = train$data[, c(vars@A, vars@W, vars@M, vars@Z, vars@Y)],
		valid = valid,
		y = vars@Y,
		continuous = continuous,
		id = NULL,
		learners = learners,
		folds = control@mlr3superlearner_folds,
		bound = FALSE
	)

	params <- list(c("data_0", "data_0", "data_0"),
								 c("data_1", "data_1", "data_1"),
								 c("data_0", "data_1", "data_1"),
								 c("data_0", "data_1", "data_0"))
	params <- lapply(params, \(x) setNames(x, c("j", "k", "l")))

	vals <- vector("list", length = 4)
	for (s in seq_along(params)) {
		j <- params[[s]]["j"]
		k <- params[[s]]["k"]
		l <- params[[s]]["l"]

		b3 <- theta3$preds[[j]]

		theta2 <- crossfit(
			train = add_psuedo(train$data[, c(vars@A, vars@W, vars@Z)], b3),
			valid = valid,
			y = "tmp_crumble_pseudo_y",
			continuous = TRUE,
			id = NULL,
			learners = learners,
			folds = control@mlr3superlearner_folds,
			bound = FALSE
		)

		b2 <- theta2$preds[[k]]

		theta1 <- crossfit(
			train = add_psuedo(train$data[, c(vars@A, vars@W)], b2),
			valid = valid,
			y = "tmp_crumble_pseudo_y",
			continuous = TRUE,
			id = NULL,
			learners = learners,
			folds = control@mlr3superlearner_folds,
			bound = FALSE
		)

		b1 <- theta1$preds[[l]]

		vals[[s]] <- list(
			fit3_weights = theta3$weights,
			fit3_natural = theta3$preds$data,
			b3 = b3,
			fit2_weights = theta2$weights,
			fit2_natural = theta2$preds$data,
			b2 = b2,
			fit1_weights = theta1$weights,
			fit1_natural = theta1$preds$data,
			b1 = b1
		)
	}

	setNames(vals, c("000", "111", "011", "010"))
}
