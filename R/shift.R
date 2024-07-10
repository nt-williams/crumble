shift_data <- function(data, trt, cens, shift) {
	if (is.null(shift)) {
		return(shift_cens(data, cens))
	}

	is_multivariate <- length(trt) > 1
	if (isTRUE(is_multivariate)) {
		return(shift_trt_multivariate(shift_cens(data, cens), trt, shift))
	}

	shift_trt_single(shift_cens(data, cens), trt, shift)
}

shift_cens <- function(data, cens) {
	if (is.na(cens)) {
		return(data)
	}

	out <- as.list(data)
	for (ce in cens) {
		out[[ce]] <- 1
	}
	as.data.frame(out, check.names = FALSE)
}

shift_trt_single <- function(data, trt, .f) {
	data[[trt]] <- .f(data, trt)
	data
}

shift_trt_multivariate <- function(data, trt, .f) {
	new <- .f(data, trt)
	for (a in trt) {
		data[[a]] <- new[[a]]
	}
	data
}
