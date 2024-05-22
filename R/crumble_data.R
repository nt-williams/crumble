crumble_data <- new_class("crumble_data",
	properties = list(
		data = new_property(class_data.frame),
		vars = new_property(new_class("crumble_vars")),
		d0 = new_property(class_function, default = NULL),
		d1 = new_property(class_function, default = NULL),
		data_0 = new_property(
			class_data.frame,
			getter = function(self) shift_data(self@data, self@vars@A, self@vars@C, self@d0)
		),
		data_1 = new_property(
			class_data.frame,
			getter = function(self) shift_data(self@data, self@vars@A, self@vars@C, self@d1)
		),
		data_0zp = new_property(class_data.frame),
		# Don't actually need this
		# data_1zp = new_property(class_data.frame)
	),
	validator = function(self) {
		all_vars <- c(self@vars@A, self@vars@W, self@vars@Z, self@vars@M, self@vars@Y)
		if (!is.na(self@vars@C)) {
			all_vars <- c(all_vars, self@vars@C)
		}

		if (!all(all_vars %in% names(self@data))) {
			"self@data must contain all variables in self@vars"
		}
	}
)

training <- new_generic("training", "x")
validation <- new_generic("validation", "x")

method(training, crumble_data) <- function(x, fold_obj, fold) {
	list(data = x@data[fold_obj[[fold]]$training_set, , drop = FALSE],
			 data_0 = x@data_0[fold_obj[[fold]]$training_set, , drop = FALSE],
			 data_1 = x@data_1[fold_obj[[fold]]$training_set, , drop = FALSE],
			 data_0zp = x@data_0zp[fold_obj[[fold]]$training_set, , drop = FALSE])
}

method(validation, crumble_data) <- function(x, fold_obj, fold) {
	list(data = x@data[fold_obj[[fold]]$validation_set, , drop = FALSE],
			 data_0 = x@data_0[fold_obj[[fold]]$validation_set, , drop = FALSE],
			 data_1 = x@data_1[fold_obj[[fold]]$validation_set, , drop = FALSE],
			 data_0zp = x@data_0zp[fold_obj[[fold]]$validation_set, , drop = FALSE])
}
