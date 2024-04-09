check_not_data_table <- function(x) {
	is_data_frame <- checkmate::checkDataFrame(x)
	if (!isTRUE(is_data_frame)) {
		return(is_data_frame)
	}

	is_data_table <- data.table::is.data.table(x)
	if (is_data_table) {
		return("Must be a 'data.frame', not a 'data.table'")
	}
	TRUE
}

assert_not_data_table <- checkmate::makeAssertionFunction(check_not_data_table)
