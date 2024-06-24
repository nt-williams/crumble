alpha_n <- function(train, valid, vars, architecture, .f, weights = NULL, control) {
	model <- nn_sequential_riesz_representer(
		train = train,
		vars = vars,
		architecture = architecture,
		.f = .f,
		weights = weights,
		learning_rate = control$learning_rate,
		epochs = control$epochs
	)

	list(train = as.numeric(model(as_torch(one_hot_encode(train[["data"]][, vars])))),
			 valid = as.numeric(model(as_torch(one_hot_encode(valid[["data"]][, vars])))))
}
