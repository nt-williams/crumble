#' @importFrom checkmate `%??%`
nn_sequential_riesz_representer <- function(train,
																						vars,
																						architecture,
																						.f,
																						weights = NULL,
																						batch_size,
																						learning_rate,
																						epochs,
																						device) {
	dataset <- make_dataset(train, vars, device = device)
	train_dl <- torch::dataloader(dataset, batch_size = batch_size)
	model <- architecture(ncol(dataset$data))
	model$to(device = device)

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
		coro::loop(for (b in train_dl) {
			# Regression loss
			loss <- (model(b$data)$pow(2) - (2 * weights * .f(model, b)))$mean(dtype = torch::torch_float())

			optimizer$zero_grad()
			loss$backward()

			optimizer$step()
		})
		scheduler$step()
		p()
	}

	model$eval()
	model
}
