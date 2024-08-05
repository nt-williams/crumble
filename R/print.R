#' @export
print.crumble <- function(x, ...) {
	switch(x$effect,
				 N = print_natural(x),
				 RT = print_rt(x),
				 O = print_organic(x),
				 RI = print_ri(x))
}

print_natural <- function(x) {
	cat("\n")
	d <- cli::cli_div(theme = list(rule = list("line-type" = "double")))
	cli::cli_rule(left = "Results {.fn crumble}")
	cli::cli_end(d)
	cli::cli_h3("{.emph E[Y(d1) - Y(d0)]}")
	cli::cli_text(cat("      "), "{.strong Estimate}: {round(x$estimates$ate, 4)}")
	cli::cli_text(cat("    "), "{.strong Std. error}: {round(calc_stderror(x$estimates$eif_ate, x$id), 4)}")
	cli::cli_text(cat("        "), "{.strong 95% CI}: ({round(calc_ci(x$estimates$ate, x$estimates$eif_ate, x$id)[1], 4)}, {round(calc_ci(x$estimates$ate, x$estimates$eif_ate, x$id)[2], 4)})")
	cli::cli_h3("{.emph Natural Direct Effect}")
	cli::cli_text(cat("      "), "{.strong Estimate}: {round(x$estimates$p1, 4)}")
	cli::cli_text(cat("    "), "{.strong Std. error}: {round(calc_stderror(x$estimates$eif_p1, x$id), 4)}")
	cli::cli_text(cat("        "), "{.strong 95% CI}: ({round(calc_ci(x$estimates$p1, x$estimates$eif_p1, x$id)[1], 4)}, {round(calc_ci(x$estimates$p1, x$estimates$eif_p1, x$id)[2], 4)})")
	cli::cli_h3("{.emph Natural Indirect Effect}")
	cli::cli_text(cat("      "), "{.strong Estimate}: {round(x$estimates$p4, 4)}")
	cli::cli_text(cat("    "), "{.strong Std. error}: {round(calc_stderror(x$estimates$eif_p4, x$id), 4)}")
	cli::cli_text(cat("        "), "{.strong 95% CI}: ({round(calc_ci(x$estimates$p4, x$estimates$eif_p4, x$id)[1], 4)}, {round(calc_ci(x$estimates$p4, x$estimates$eif_p4, x$id)[2], 4)})")
}

print_rt <- function(x) {
	cat("\n")
	d <- cli::cli_div(theme = list(rule = list("line-type" = "double")))
	cli::cli_rule(left = "Results {.fn crumble}")
	cli::cli_end(d)
	cli::cli_h3("{.emph E[Y(d1) - Y(d0)]}")
	cli::cli_text(cat("      "), "{.strong Estimate}: {round(x$estimates$ate, 4)}")
	cli::cli_text(cat("    "), "{.strong Std. error}: {round(calc_stderror(x$estimates$eif_ate, x$id), 4)}")
	cli::cli_text(cat("        "), "{.strong 95% CI}: ({round(calc_ci(x$estimates$ate, x$estimates$eif_ate, x$id)[1], 4)}, {round(calc_ci(x$estimates$ate, x$estimates$eif_ate, x$id)[2], 4)})")
	cli::cli_h3("{.emph Path: A -> Y}")
	cli::cli_text(cat("      "), "{.strong Estimate}: {round(x$estimates$p1, 4)}")
	cli::cli_text(cat("    "), "{.strong Std. error}: {round(calc_stderror(x$estimates$eif_p1, x$id), 4)}")
	cli::cli_text(cat("        "), "{.strong 95% CI}: ({round(calc_ci(x$estimates$p1, x$estimates$eif_p1, x$id)[1], 4)}, {round(calc_ci(x$estimates$p1, x$estimates$eif_p1, x$id)[2], 4)})")
	cli::cli_h3("{.emph Path: A -> Z -> Y}")
	cli::cli_text(cat("      "), "{.strong Estimate}: {round(x$estimates$p2, 4)}")
	cli::cli_text(cat("    "), "{.strong Std. error}: {round(calc_stderror(x$estimates$eif_p2, x$id), 4)}")
	cli::cli_text(cat("        "), "{.strong 95% CI}: ({round(calc_ci(x$estimates$p2, x$estimates$eif_p2, x$id)[1], 4)}, {round(calc_ci(x$estimates$p2, x$estimates$eif_p2, x$id)[2], 4)})")
	cli::cli_h3("{.emph Path: A -> Z -> M -> Y}")
	cli::cli_text(cat("      "), "{.strong Estimate}: {round(x$estimates$p3, 4)}")
	cli::cli_text(cat("    "), "{.strong Std. error}: {round(calc_stderror(x$estimates$eif_p3, x$id), 4)}")
	cli::cli_text(cat("        "), "{.strong 95% CI}: ({round(calc_ci(x$estimates$p3, x$estimates$eif_p3, x$id)[1], 4)}, {round(calc_ci(x$estimates$p3, x$estimates$eif_p3, x$id)[2], 4)})")
	cli::cli_h3("{.emph Path: A -> M -> Y}")
	cli::cli_text(cat("      "), "{.strong Estimate}: {round(x$estimates$p4, 4)}")
	cli::cli_text(cat("    "), "{.strong Std. error}: {round(calc_stderror(x$estimates$eif_p4, x$id), 4)}")
	cli::cli_text(cat("        "), "{.strong 95% CI}: ({round(calc_ci(x$estimates$p4, x$estimates$eif_p4, x$id)[1], 4)}, {round(calc_ci(x$estimates$p4, x$estimates$eif_p4, x$id)[2], 4)})")
	cli::cli_h3("{.emph Intermediate Confounding}")
	cli::cli_text(cat("      "), "{.strong Estimate}: {round(x$estimates$intermediate_confounding, 4)}")
	cli::cli_text(cat("    "), "{.strong Std. error}: {round(calc_stderror(x$estimates$eif_intermediate_confounding, x$id), 4)}")
	cli::cli_text(cat("        "), "{.strong 95% CI}: ({round(calc_ci(x$estimates$intermediate_confounding, x$estimates$eif_intermediate_confounding, x$id)[1], 4)}, {round(calc_ci(x$estimates$intermediate_confounding, x$estimates$eif_intermediate_confounding, x$id)[2], 4)})")
}

print_organic <- function(x) {
	cat("\n")
	d <- cli::cli_div(theme = list(rule = list("line-type" = "double")))
	cli::cli_rule(left = "Results {.fn crumble}")
	cli::cli_end(d)
	cli::cli_h3("{.emph Organic Direct Effect}")
	cli::cli_text(cat("      "), "{.strong Estimate}: {round(x$estimates$ode, 4)}")
	cli::cli_text(cat("    "), "{.strong Std. error}: {round(calc_stderror(x$estimates$eif_ode, x$id), 4)}")
	cli::cli_text(cat("        "), "{.strong 95% CI}: ({round(calc_ci(x$estimates$ode, x$estimates$eif_ode, x$id)[1], 4)}, {round(calc_ci(x$estimates$ode, x$estimates$eif_ode, x$id)[2], 4)})")
	cli::cli_h3("{.emph Organic Indirect Effect}")
	cli::cli_text(cat("      "), "{.strong Estimate}: {round(x$estimates$oie, 4)}")
	cli::cli_text(cat("    "), "{.strong Std. error}: {round(calc_stderror(x$estimates$eif_oie, x$id), 4)}")
	cli::cli_text(cat("        "), "{.strong 95% CI}: ({round(calc_ci(x$estimates$oie, x$estimates$eif_oie, x$id)[1], 4)}, {round(calc_ci(x$estimates$oie, x$estimates$eif_oie, x$id)[2], 4)})")
}

print_ri <- function(x) {
	cat("\n")
	d <- cli::cli_div(theme = list(rule = list("line-type" = "double")))
	cli::cli_rule(left = "Results {.fn crumble}")
	cli::cli_end(d)
	cli::cli_h3("{.emph Randomized Direct Effect}")
	cli::cli_text(cat("      "), "{.strong Estimate}: {round(x$estimates$ride, 4)}")
	cli::cli_text(cat("    "), "{.strong Std. error}: {round(calc_stderror(x$estimates$eif_ride, x$id), 4)}")
	cli::cli_text(cat("        "), "{.strong 95% CI}: ({round(calc_ci(x$estimates$ride, x$estimates$eif_ride, x$id)[1], 4)}, {round(calc_ci(x$estimates$ride, x$estimates$eif_ride, x$id)[2], 4)})")
	cli::cli_h3("{.emph Randomized Indirect Effect}")
	cli::cli_text(cat("      "), "{.strong Estimate}: {round(x$estimates$riie, 4)}")
	cli::cli_text(cat("    "), "{.strong Std. error}: {round(calc_stderror(x$estimates$eif_riie, x$id), 4)}")
	cli::cli_text(cat("        "), "{.strong 95% CI}: ({round(calc_ci(x$estimates$riie, x$estimates$eif_riie, x$id)[1], 4)}, {round(calc_ci(x$estimates$ride, x$estimates$eif_riie, x$id)[2], 4)})")
}
