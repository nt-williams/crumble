crumble_data <- S7::new_class("crumble_data",
	properties = list(
		data = S7::new_property(S7::class_data.frame),
		vars = S7::new_property(S7::new_class("crumble_vars")),
		weights = S7::new_property(S7::class_numeric),
		d0 = S7::new_property(S7::class_function, default = NULL),
		d1 = S7::new_property(S7::class_function, default = NULL),
		data_0 = S7::new_property(S7::class_data.frame),
		data_1 = S7::new_property(S7::class_data.frame),
		data_0zp = S7::new_property(S7::class_data.frame),
		data_1zp = S7::new_property(S7::class_data.frame)
	),
	constructor = function(data, vars, weights, d0, d1) {
		if (!no_Z(vars)) {
			z_ohe <- one_hot_encode(data, vars@Z)
			vars@Z <- names(z_ohe)
			data <- data[, !(names(data) %in% vars@Z)]
			data <- cbind(data, z_ohe)
		}
		S7::new_object(
			S7::S7_object(),
			data = data,
			vars = vars,
			weights = normalize(weights),
			d0 = d0,
			d1 = d1,
			data_0 = shift_data(data, vars@A, vars@C, d0),
			data_1 = shift_data(data, vars@A, vars@C, d1),
			data_0zp = data.frame(),
			data_1zp = data.frame()
		)
	},
	validator = function(self) {
		all_vars <- c(self@vars@A, self@vars@W, self@vars@Z, self@vars@M, self@vars@C, self@vars@Y)
		all_vars <- as.vector(na.omit(all_vars))

		if (!all(all_vars %in% names(self@data))) {
			"self@data must contain all variables in self@vars"
		}
	}
)

training <- S7::new_generic("training", "x")
validation <- S7::new_generic("validation", "x")

S7::method(training, crumble_data) <- function(x, fold_obj, fold) {
	list(
		data = x@data[fold_obj[[fold]]$training_set, , drop = FALSE],
		data_0 = x@data_0[fold_obj[[fold]]$training_set, , drop = FALSE],
		data_1 = x@data_1[fold_obj[[fold]]$training_set, , drop = FALSE],
		data_0zp = x@data_0zp[fold_obj[[fold]]$training_set, , drop = FALSE],
		data_1zp = x@data_1zp[fold_obj[[fold]]$training_set, , drop = FALSE]
	)
}

S7::method(validation, crumble_data) <- function(x, fold_obj, fold) {
	list(
		data = x@data[fold_obj[[fold]]$validation_set, , drop = FALSE],
		data_0 = x@data_0[fold_obj[[fold]]$validation_set, , drop = FALSE],
		data_1 = x@data_1[fold_obj[[fold]]$validation_set, , drop = FALSE],
		data_0zp = x@data_0zp[fold_obj[[fold]]$validation_set, , drop = FALSE],
		data_1zp = x@data_1zp[fold_obj[[fold]]$validation_set, , drop = FALSE]
	)
}
