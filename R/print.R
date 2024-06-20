#' @export
print.crumble <- function(x, ...) {
	cat("\n")
	d <- cli::cli_div(theme = list(rule = list("line-type" = "double")))
	cli::cli_rule(left = "Results {.fn crumble}")
	cli::cli_end(d)
	cli::cli_h3("{.emph Comparitive Treatment Effect |-> d1 - d0}")
	cli::cli_text(cat("      "), "{.strong Estimate}: {round(x$estimates$ate, 4)}")
	cli::cli_text(cat("    "), "{.strong Std. error}: {round(calc_stderror(x$estimates$eif_ate), 4)}")
	cli::cli_text(cat("        "), "{.strong 95% CI}: ({round(calc_ci(x$estimates$ate, x$estimates$eif_ate)[1], 4)}, {round(calc_ci(x$estimates$ate, x$estimates$eif_ate)[2], 4)})")
	cli::cli_h3("{.emph Path: A -> Y}")
	cli::cli_text(cat("      "), "{.strong Estimate}: {round(x$estimates$p1, 4)}")
	cli::cli_text(cat("    "), "{.strong Std. error}: {round(calc_stderror(x$estimates$eif_p1), 4)}")
	cli::cli_text(cat("        "), "{.strong 95% CI}: ({round(calc_ci(x$estimates$p1, x$estimates$eif_p1)[1], 4)}, {round(calc_ci(x$estimates$p1, x$estimates$eif_p1)[2], 4)})")
	if (isFALSE(x$natural)) {
		cli::cli_h3("{.emph Path: A -> Z -> Y}")
		cli::cli_text(cat("      "), "{.strong Estimate}: {round(x$estimates$p2, 4)}")
		cli::cli_text(cat("    "), "{.strong Std. error}: {round(calc_stderror(x$estimates$eif_p2), 4)}")
		cli::cli_text(cat("        "), "{.strong 95% CI}: ({round(calc_ci(x$estimates$p2, x$estimates$eif_p2)[1], 4)}, {round(calc_ci(x$estimates$p2, x$estimates$eif_p2)[2], 4)})")
		cli::cli_h3("{.emph Path: A -> Z -> M -> Y}")
		cli::cli_text(cat("      "), "{.strong Estimate}: {round(x$estimates$p3, 4)}")
		cli::cli_text(cat("    "), "{.strong Std. error}: {round(calc_stderror(x$estimates$eif_p3), 4)}")
		cli::cli_text(cat("        "), "{.strong 95% CI}: ({round(calc_ci(x$estimates$p3, x$estimates$eif_p3)[1], 4)}, {round(calc_ci(x$estimates$p3, x$estimates$eif_p3)[2], 4)})")
	}
	cli::cli_h3("{.emph Path: A -> M -> Y}")
	cli::cli_text(cat("      "), "{.strong Estimate}: {round(x$estimates$p4, 4)}")
	cli::cli_text(cat("    "), "{.strong Std. error}: {round(calc_stderror(x$estimates$eif_p4), 4)}")
	cli::cli_text(cat("        "), "{.strong 95% CI}: ({round(calc_ci(x$estimates$p4, x$estimates$eif_p4)[1], 4)}, {round(calc_ci(x$estimates$p4, x$estimates$eif_p4)[2], 4)})")
	if (isFALSE(x$natural)) {
		cli::cli_h3("{.emph Intermediate Confounding}")
		cli::cli_text(cat("      "), "{.strong Estimate}: {round(x$estimates$intermediate_confounding, 4)}")
		cli::cli_text(cat("    "), "{.strong Std. error}: {round(calc_stderror(x$estimates$eif_intermediate_confounding), 4)}")
		cli::cli_text(cat("        "), "{.strong 95% CI}: ({round(calc_ci(x$estimates$intermediate_confounding, x$estimates$eif_intermediate_confounding)[1], 4)}, {round(calc_ci(x$estimates$intermediate_confounding, x$estimates$eif_intermediate_confounding)[2], 4)})")
	}
}
