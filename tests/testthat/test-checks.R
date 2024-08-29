test_that("detects missing variables", {
	foo <- data.frame(a = 1, w = 1, m = 1, z = NA, y = 1)
	expect_error(
		crumble(foo, "a", "y", "m", "z", "w"),
		"Assertion on 'data' failed: Missing data found in treatment/covariate/mediator/observed nodes."
	)
})
