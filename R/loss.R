loss_riesz <- function(datalist, task, alpha, .f) {
	datalist <- lapply(datalist, function(x) {
		datalist[, task$vars]
	})

	(alpha(datalist[["natural"]])$pow(2) - (2*.f(alpha, datalist)))$mean(dtype = torch::torch_float())
}

