library(proxy)

data <- matrix(rnorm(20), nrow=3)
data <- matrix(c(0.8147, 0.9134, 0.2785, 0.9058, 0.6324, 0.5469, 0.1270, 0.0975, 0.9575),
							 nrow = 3,
							 byrow = TRUE)

data <- gendata(1e3)
z <- data[, "z", drop = FALSE] |> as.matrix()

# Compute the pairwise distance matrix with a custom distance function
distance_matrix <- proxy::dist(z, method = "euclidean")

# Convert the distance matrix to a vector
distance_vector <- as.vector(as.matrix(distance_matrix))

# Calculate the median of the pairwise distances
median_distance <- median(distance_vector)

# Print the result
print(median_distance)

kernel <- rbf(median_distance)

D <- rkhs_distance(data, kernel)

n <- nrow(D)
# rescale distances
D <- D / max(D)
# objective function
f <- as.vector(D)
# Inequality constraint
lb <- matrix(0, nrow = n^2)
# Equality constraints
Aeq <- Matrix::Matrix(0, nrow = 2*n, ncol = n^2)
b <- matrix(1, nrow = 2*n)

# Columns sum to 1
for (c in 0:(n-1)) {
	Aeq[c + 1, (c * n + 1):((c + 1) * n)] <- 1
}

# Rows sum to 1
for (r in 1:(n-1)) {
	for (c in 1:n) {
		Aeq[r + n, (c - 1) * n + r] <- 1
	}
}

# Diagonal entries zero
for (z in 1:n) {
	Aeq[2*n, (z - 1)*(n + 1) + 1] <- 1
}

b[2*n, 1] <- 0

P <- lp("min", f, as.matrix(Aeq), rep("=", length(b)), b)
matrix(P$solution, nrow = n)



