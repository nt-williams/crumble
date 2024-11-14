#' @importFrom cli cli_div cli_rule cli_end cli_h3
#' @importFrom purrr iwalk
#' @export
print.crumble <- function(x, ...) {
	cat("\n")
	d <- cli_div(theme = list(rule = list("line-type" = "double")))
	cli_rule(left = "Results {.fn crumble}")
	cli_end(d)
	iwalk(x$estimates, print_estimate)
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
	cli_h3("{.emph {title}}")
	print(x)
