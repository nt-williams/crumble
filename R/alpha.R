alpha_n <- function(train, valid, vars, architecture, .f, weights = NULL, control) {
	model <- nn_sequential_riesz_representer(
		train = train,
		vars = vars,
		architecture = architecture,
		.f = .f,
		weights = weights,
		learning_rate = control@learning_rate,
		epochs = control@epochs
	)

	as.numeric(model(as_torch(valid[["data"]][, vars])))
}
