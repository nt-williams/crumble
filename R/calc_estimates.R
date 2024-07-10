calc_estimates_natural <- function(eif_ns) {
	list(
		# A -> Y
		p1 = mean(eif_ns[, "100"] - eif_ns[, "000"]),
		eif_p1 = eif_ns[, "100"] - eif_ns[, "000"],
		# A -> M -> Y
		p4 = mean(eif_ns[, "111"] - eif_ns[, "100"]),
		eif_p4 = eif_ns[, "111"] - eif_ns[, "100"],
		ate = mean(eif_ns[, "111"] - eif_ns[, "000"]),
		eif_ate = eif_ns[, "111"] - eif_ns[, "000"]
	)
}

calc_estimates_organic <- function(eif_ns) {
	list(
		ode = mean(eif_ns[, "101"] - eif_ns[, "000"]),
		eif_ode = eif_ns[, "101"] - eif_ns[, "000"],
		oie = mean(eif_ns[, "111"] - eif_ns[, "101"]),
		eif_oie = eif_ns[, "111"] - eif_ns[, "101"]
	)
}

calc_estimates_ri <- function(eif_rs) {
	list(
		ride = mean(eif_rs[, "1100"] - eif_rs[, "0000"]),
		eif_ride = eif_rs[, "1100"] - eif_rs[, "0000"],
		riie = mean(eif_rs[, "1111"] - eif_rs[, "1100"]),
		eif_riie = eif_rs[, "1111"] - eif_rs[, "1100"]
	)
}

calc_estimates_rt <- function(eif_ns, eif_rs) {
	list(
		# A -> Y
		p1 = mean(eif_ns[, "111"] - eif_ns[, "011"]),
		eif_p1 = eif_ns[, "111"] - eif_ns[, "011"],
		# A -> Z -> Y
		p2 = mean(eif_rs[, "0111"] - eif_rs[, "0011"]),
		eif_p2 = eif_rs[, "0111"] - eif_rs[, "0011"],
		# A -> Z -> M -> Y
		p3 = mean(eif_rs[, "0011"] - eif_rs[, "0010"]),
		eif_p3 = eif_rs[, "0011"] - eif_rs[, "0010"],
		# A -> M -> Y
		p4 = mean(eif_ns[, "010"] - eif_ns[, "000"]),
		eif_p4 = eif_ns[, "010"] - eif_ns[, "000"],
		# Intermediate confounding
		intermediate_confounding = mean(
			eif_ns[, "011"] - eif_rs[, "0111"] +
				eif_rs[, "0011"] - eif_rs[, "0011"] +
				eif_rs[, "0010"] - eif_ns[, "010"]
		),
		eif_intermediate_confounding =
			eif_ns[, "011"] - eif_rs[, "0111"] +
			eif_rs[, "0011"] - eif_rs[, "0011"] +
			eif_rs[, "0010"] - eif_ns[, "010"],
		ate = mean(eif_ns[, "111"] - eif_ns[, "000"]),
		eif_ate = eif_ns[, "111"] - eif_ns[, "000"]
	)
}
