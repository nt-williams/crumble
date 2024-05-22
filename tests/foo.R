library(dplyr)

data <- gendata(1e3)

crumble(data, "a", "y", "m", "z", "w",
				d0 = lmtp::static_binary_off,
				d1 = lmtp::static_binary_on)

d <- list("0" = 0, "1" = 1)

fit_theta3 <- glm(y ~ ., data = data, family = "binomial")
b3 <- predict(fit_theta3, mutate(data, a = d[[j]]), type = "response")

fit_theta2 <- glm(b3 ~ a + z + w, data = cbind(b3, data))
b2 <- predict(fit_theta2, mutate(data, a = d[[k]]))

fit_theta1 <- glm(b2 ~ a + w, data = cbind(b2, data))
b1 <- predict(fit_theta1, data = mutate(data, a = d[[l]]))

var_order <- seq_along(data)
names(var_order) <- names(data)

datalist <- list("data" = data,
								 "data_0" = mutate(data, a = 0),
								 "data_1" = mutate(data, a = 1))

alpha_000 <- phi_n_alpha(datalist, "data_0", "data_0", "data_0")
alpha_111 <- phi_n_alpha(datalist, "data_1", "data_1", "data_1")
alpha_011 <- phi_n_alpha(datalist, "data_0", "data_1", "data_1")
alpha_010 <- phi_n_alpha(datalist, "data_0", "data_1", "data_0")

vars <- crumble_vars("a", "y", "m", "z", "w")
tns <- theta_n(datalist, vars, "binomial", "glm", NULL)

psin_000 <- mean(alpha_000$alpha3$response * (data$y - tns$`000`$fit3$preds$natural) +
alpha_000$alpha2$response * (tns$`000`$b3 - tns$`000`$fit2$preds$natural) +
alpha_000$alpha1$response * (tns$`000`$b2 - tns$`000`$fit1$preds$natural) +
	tns$`000`$b1)

psin_111 <- mean(alpha_111$alpha3$response * (data$y - tns$`111`$fit3$preds$natural) +
	alpha_111$alpha2$response * (tns$`111`$b3 - tns$`111`$fit2$preds$natural) +
	alpha_111$alpha1$response * (tns$`111`$b2 - tns$`111`$fit1$preds$natural) +
	tns$`111`$b1)

psin_011 <- mean(alpha_011$alpha3$response * (data$y - tns$`011`$fit3$preds$natural) +
	alpha_011$alpha2$response * (tns$`011`$b3 - tns$`011`$fit2$preds$natural) +
	alpha_011$alpha1$response * (tns$`011`$b2 - tns$`011`$fit1$preds$natural) +
	tns$`011`$b1)

psin_010 <- mean(alpha_010$alpha3$response * (data$y - tns$`010`$fit3$preds$natural) +
	alpha_010$alpha2$response * (tns$`010`$b3 - tns$`010`$fit2$preds$natural) +
	alpha_010$alpha1$response * (tns$`010`$b2 - tns$`010`$fit1$preds$natural) +
	tns$`010`$b1)

psin_111 - psin_011
psin_010 - psin_000
