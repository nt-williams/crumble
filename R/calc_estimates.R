calc_estimates_natural <- function(eif_ns, weights) {
	weights <- weights %??% rep(1, nrow(eif_ns))
	list(
		# A -> Y
		p1 = weighted.mean(eif_ns[, "100"] - eif_ns[, "000"], weights),
		eif_p1 = eif_ns[, "100"] - eif_ns[, "000"],
		# A -> M -> Y
		p4 = weighted.mean(eif_ns[, "111"] - eif_ns[, "100"], weights),
		eif_p4 = eif_ns[, "111"] - eif_ns[, "100"],
		ate = weighted.mean(eif_ns[, "111"] - eif_ns[, "000"], weights),
		eif_ate = eif_ns[, "111"] - eif_ns[, "000"]
	)
}

calc_estimates_organic <- function(eif_ns, weights) {
	weights <- weights %??% rep(1, nrow(eif_ns))
	list(
		ode = weighted.mean(eif_ns[, "101"] - eif_ns[, "000"], weights),
		eif_ode = eif_ns[, "101"] - eif_ns[, "000"],
		oie = weighted.mean(eif_ns[, "111"] - eif_ns[, "101"], weights),
		eif_oie = eif_ns[, "111"] - eif_ns[, "101"]
	)
}

calc_estimates_ri <- function(eif_rs, weights) {
	weights <- weights %??% rep(1, nrow(eif_rs))
	list(
		ride = weighted.mean(eif_rs[, "1100"] - eif_rs[, "0000"], weights),
		eif_ride = eif_rs[, "1100"] - eif_rs[, "0000"],
		riie = weighted.mean(eif_rs[, "1111"] - eif_rs[, "1100"], weights),
		eif_riie = eif_rs[, "1111"] - eif_rs[, "1100"]
	)
}

calc_estimates_rt <- function(eif_ns, eif_rs, weights) {
	weights <- weights %??% rep(1, nrow(eif_ns))
	list(
		# A -> Y
		p1 = weighted.mean(eif_ns[, "111"] - eif_ns[, "011"], weights),
		eif_p1 = eif_ns[, "111"] - eif_ns[, "011"],
		# A -> Z -> Y
		p2 = weighted.mean(eif_rs[, "0111"] - eif_rs[, "0011"], weights),
		eif_p2 = eif_rs[, "0111"] - eif_rs[, "0011"],
		# A -> Z -> M -> Y
		p3 = weighted.mean(eif_rs[, "0011"] - eif_rs[, "0010"], weights),
		eif_p3 = eif_rs[, "0011"] - eif_rs[, "0010"],
		# A -> M -> Y
		p4 = weighted.mean(eif_ns[, "010"] - eif_ns[, "000"], weights),
		eif_p4 = eif_ns[, "010"] - eif_ns[, "000"],
		# Intermediate confounding
		intermediate_confounding = weighted.mean(
			eif_ns[, "011"] - eif_rs[, "0111"] +
				eif_rs[, "0011"] - eif_rs[, "0011"] +
				eif_rs[, "0010"] - eif_ns[, "010"], weights
		),
		eif_intermediate_confounding =
			eif_ns[, "011"] - eif_rs[, "0111"] +
			eif_rs[, "0011"] - eif_rs[, "0011"] +
			eif_rs[, "0010"] - eif_ns[, "010"],
		ate = weighted.mean(eif_ns[, "111"] - eif_ns[, "000"], weights),
		eif_ate = eif_ns[, "111"] - eif_ns[, "000"]
	)
}
