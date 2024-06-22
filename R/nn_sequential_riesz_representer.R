#' @importFrom checkmate `%??%`
nn_sequential_riesz_representer <- function(train, vars, architecture, .f, weights = NULL,
																						learning_rate = 1e-3, epochs = 500) {
	data <- as_torch(one_hot_encode(train$data[, vars]))
	model <- architecture(data)

	weights <- weights %??% 1

	optimizer <- torch::optim_adam(
		params = c(model$parameters),
		lr = learning_rate,
		weight_decay = 0.01
	)

	scheduler <- torch::lr_one_cycle(
		optimizer,
		max_lr = learning_rate,
		total_steps = epochs
	)

	p <- progressr::progressor(steps = epochs)

	for (epoch in 1:epochs) {
		# Regression loss
		loss <- (model(data)$pow(2) - (2 * weights * .f(model, train)))$mean(dtype = torch::torch_float())

		optimizer$zero_grad()
		loss$backward()

		optimizer$step()
		scheduler$step()
		p()
	}

	model$eval()
	model
}
