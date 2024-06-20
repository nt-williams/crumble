crumble_data <- new_class("crumble_data",
	properties = list(
		data = new_property(class_data.frame),
		vars = new_property(new_class("crumble_vars")),
		d0 = new_property(class_function, default = NULL),
		d1 = new_property(class_function, default = NULL),
		data_0 = new_property(class_data.frame),
		data_1 = new_property(class_data.frame),
		data_0zp = new_property(class_data.frame)
		# Don't need data_1zp
	),
	constructor = function(data, vars, d0, d1) {
		new_object(S7_object(),
							 data = data,
							 vars = vars,
							 d0 = d0,
							 d1 = d1,
							 data_0 = shift_data(data, vars@A, vars@C, d0),
							 data_1 = shift_data(data, vars@A, vars@C, d1),
							 data_0zp = class_missing)
	},
	validator = function(self) {
		all_vars <- c(self@vars@A, self@vars@W, self@vars@Z, self@vars@M, self@vars@C, self@vars@Y)
		all_vars <- as.vector(na.omit(all_vars))

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
