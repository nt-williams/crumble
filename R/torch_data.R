as_torch_datalist <- function(data, d1, d2) {
	datalist <- setNames(lapply(list(data, d1, d2), as_torch), c("natural", "d1", "d2"))
	function(key) {
		datalist[[key]]
	}
}

as_torch <- function(data) {
	torch::torch_tensor(as.matrix(data), dtype = torch::torch_float())
}
