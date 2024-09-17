#' @importFrom generics tidy
#' @export
generics::tidy

#' Tidy a(n) crumble object
#'
#' @param x A `crumble` object produced by a call to [crumble::crumble()].
#' @param ... Unused, included for generic consistency only.
#'
#' @return A tidy [tibble::tibble()] summarizing information about the model.
#'
#' @example inst/examples/examples.R
#'
#' @export
tidy.crumble <- function(x, ...) {
	out <- switch(x$effect,
								N = tidy_natural(x),
								RT = tidy_rt(x),
								O = tidy_organic(x),
								RI = tidy_ri(x))
	class(out) <- c("tbl_df", "tbl", "data.frame")
	out
}

tidy_natural <- function(x) {
	out <- data.frame(
		effect_type = x$effect,
		estimand = c("total effect", "natural direct effect", "natural indirect effect"),
		estimate = c(x$estimates$ate, x$estimates$p1, x$estimates$p4)
	)

	out$std.error <- mapply(
		calc_stderror,
		list(x$estimates$eif_ate, x$estimates$eif_p1, x$estimates$eif_p4),
		MoreArgs = list(id = x$id, weights = x$weights), SIMPLIFY = TRUE
	)

	ci <- mapply(
		calc_ci,
		out$estimate,
		list(x$estimates$eif_ate, x$estimates$eif_p1, x$estimates$eif_p4),
		MoreArgs = list(id = x$id, weights = x$weights),
		SIMPLIFY = FALSE
	)

	out$conf.low <- sapply(ci, function(x) x[1])
	out$conf.high <- sapply(ci, function(x) x[2])
	out
}

tidy_rt <- function(x) {
	out <- data.frame(
		effect_type = x$effect,
		estimand = c("total effect", paste0("p", 1:4), "intermediate confounding"),
		estimate = unlist(lapply(c("ate", paste0("p", 1:4), "intermediate_confounding"),
														 \(i) x$estimates[[i]]))
	)

	out$std.error <- mapply(
		calc_stderror,
		lapply(c("eif_ate", paste0("eif_p", 1:4), "eif_intermediate_confounding"), \(i) x$estimates[[i]]),
		MoreArgs = list(id = x$id, weights = x$weights), SIMPLIFY = TRUE
	)

	ci <- mapply(
		calc_ci,
		out$estimate,
		lapply(c("eif_ate", paste0("eif_p", 1:4), "eif_intermediate_confounding"), \(i) x$estimates[[i]]),
		MoreArgs = list(id = x$id, weights = x$weights),
		SIMPLIFY = FALSE
	)

	out$conf.low <- sapply(ci, function(x) x[1])
	out$conf.high <- sapply(ci, function(x) x[2])
	out
}

tidy_organic <- function(x) {
	out <- data.frame(
		effect_type = x$effect,
		estimand = c("organic direct effect", "organic indirect effect"),
		estimate = c(x$estimates$ode, x$estimates$oie)
	)

	out$std.error <- mapply(
		calc_stderror,
		list(x$estimates$eif_ode, x$estimates$eif_oie),
		MoreArgs = list(id = x$id, weights = x$weights), SIMPLIFY = TRUE
	)

	ci <- mapply(
		calc_ci,
		out$estimate,
		list(x$estimates$eif_ode, x$estimates$eif_oie),
		MoreArgs = list(id = x$id, weights = x$weights),
		SIMPLIFY = FALSE
	)

	out$conf.low <- sapply(ci, function(x) x[1])
	out$conf.high <- sapply(ci, function(x) x[2])
	out
}

tidy_ri <- function(x) {
	out <- data.frame(
		effect_type = x$effect,
		estimand = c("randomized direct effect", "randomized indirect effect"),
		estimate = c(x$estimates$ride, x$estimates$riie)
	)

	out$std.error <- mapply(
		calc_stderror,
		list(x$estimates$eif_ride, x$estimates$eif_riie),
		MoreArgs = list(id = x$id, weights = x$weights), SIMPLIFY = TRUE
	)

	ci <- mapply(
		calc_ci,
		out$estimate,
		list(x$estimates$eif_ride, x$estimates$eif_riie),
		MoreArgs = list(id = x$id, weights = x$weights),
		SIMPLIFY = FALSE
	)

	out$conf.low <- sapply(ci, function(x) x[1])
	out$conf.high <- sapply(ci, function(x) x[2])
	out
}
