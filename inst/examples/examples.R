\donttest{
if (require("mma")) {
	data(weight_behavior, package = "mma")

	weight_behavior <- na.omit(weight_behavior)

	crumble(
		data = weight_behavior,
		trt = "sports",
		outcome = "bmi",
		covar = c("age", "sex", "tvhours"),
		mediators = c("exercises", "overweigh"),
		moc = "snack",
		d0 = \(data, trt) factor(rep(1, nrow(data)), levels = c("1", "2")),
		d1 = \(data, trt) factor(rep(2, nrow(data)), levels = c("1", "2")),
		learners = c("mean", "glm"),
		nn_module = sequential_module(),
		control = crumble_control(crossfit_folds = 1L, zprime_folds = 5L)
	)
}
}
