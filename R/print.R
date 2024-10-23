#' @export
print.crumble <- function(x, ...) {
	cat("\n")
	d <- cli::cli_div(theme = list(rule = list("line-type" = "double")))
	cli::cli_rule(left = "Results {.fn crumble}")
	cli::cli_end(d)
	purrr::iwalk(x$estimates, print_estimate)
}

print_estimate <- function(x, name) {
	title <- switch(name,
									"ate" = "Average Treatment Effect",
									"direct" = "Direct Effect",
									"indirect" = "Indirect Effect",
									"p1" = "Path: A -> Y",
									"p2" = "Path: A -> Z -> Y",
									"p3" = "Path: A -> Z -> M -> Y",
									"p4" = "Path: A -> M -> Y",
									"intermediate_confounding" = "Intermediate Confounding",
									"ode" = "Organic Direct Effect",
									"oie" = "Organic Indirect Effect",
									"ride" = "Randomized Direct Effect",
									"riie" = "Randomized Indirect Effect")
	cli::cli_h3("{.emph {title}}")
	print(x)
}
