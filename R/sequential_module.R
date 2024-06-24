#' Title
#'
#' @param layers
#' @param hidden
#' @param dropout
#'
#' @return
#' @export
#'
#' @examples
sequential_module <- function(layers = 1, hidden = 20, dropout = 0.1) {
	function(data) {
		# input dimensionality
		d_in <- ncol(data)
		# output dimensionality
		d_out <- 1

		middle_layers <- lapply(1:layers, \(x) torch::nn_sequential(torch::nn_linear(hidden, hidden), torch::nn_elu()))

		torch::nn_sequential(
			torch::nn_linear(d_in, hidden),
			torch::nn_elu(),
			do.call(torch::nn_sequential, middle_layers),
			torch::nn_linear(hidden, d_out),
			torch::nn_dropout(dropout),
			torch::nn_softplus()
		)
	}
}
