Alpha <- function(train, valid, vars, architecture, .f, weights = NULL, control) {
	model <- nn_sequential_riesz_representer(
		train = train,
		vars = vars,
		architecture = architecture,
		.f = .f,
		weights = weights,
		batch_size = control$batch_size,
		learning_rate = control$learning_rate,
		epochs = control$epochs,
		device = control$device
	)

	list(
		train = as.numeric(
			model(
				as_torch(
					one_hot_encode(train[["data"]][, vars]),
					device = control$device
				)
			)
		),
		valid = as.numeric(
			model(
				as_torch(
					one_hot_encode(valid[["data"]][, vars]),
					device = control$device
				)
			)
		)
	)
}
