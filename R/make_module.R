make_module <- function(data) {
	d_in <- ncol(one_hot_encode(data))
	hidden <- ceiling(d_in / 2)
	torch::nn_sequential(
		torch::nn_linear(d_in, hidden),
		torch::nn_relu(),
		torch::nn_linear(hidden, hidden),
		torch::nn_relu(),
		torch::nn_linear(hidden, hidden),
		torch::nn_relu(),
		torch::nn_linear(hidden, 1),
		torch::nn_softplus()
	)
}
