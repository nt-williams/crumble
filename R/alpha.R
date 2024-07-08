alpha_n <- function(train, valid, vars, architecture, .f, weights = NULL, control) {
	# dataset <- make_dataset(train, vars)
	# browser()
	# It would be more efficient to just do dataset creation here on the whole data,
	# and then make the splits
	model <- nn_sequential_riesz_representer(
		train = train,
		vars = vars,
		architecture = architecture,
		.f = .f,
		weights = weights,
		batch_size = control$batch_size,
		learning_rate = control$learning_rate,
		epochs = control$epochs
	)

	list(train = as.numeric(model(as_torch(one_hot_encode(train[["data"]][, vars])))),
			 valid = as.numeric(model(as_torch(one_hot_encode(valid[["data"]][, vars])))))
}
