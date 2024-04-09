make_folds <- function(data, V) {
	folds <- origami::make_folds(data, V = V)
	if (V == 1) {
		folds[[1]]$training_set <- folds[[1]]$validation_set
	}
	folds
}

convert_NA <- function(data, vars) {
	if (is.na(vars@C)) {
		return(data)
	}

	data[[vars@Y]] <- ifelse(is.na(data[[vars@Y]]), -999, data[[vars@Y]])
	data
}

set_trt <- function(data, vars, value) {
	data[[vars@A]] <- value
	data
}
