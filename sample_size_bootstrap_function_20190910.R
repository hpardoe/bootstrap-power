# bootstrap
sample.size.bootstrap <- function(sample.vector, effect.size = 0.1, low.n = 5, high.n = 300, by.n = 5) {
  n.vector <- seq(from = low.n, to = high.n, by = by.n)
  power.bootstrap.vector <- vector(length = length(n.vector))
  for (i in 1:length(n.vector)) {
    n.iterations <- 5000
    iter.vector <- vector(length = n.iterations)
    for (j in 1:length(iter.vector)) {
      #my.dist.01 <- rnorm(n = n.vector[i], mean = 0, sd = my.sd)
      my.dist.01 <- sample(sample.vector, size = n.vector[i], replace = TRUE)
      #my.dist.02 <- rnorm(n = n.vector[i], mean = my.effect, sd = my.sd)
      my.dist.02 <- sample(sample.vector, size = n.vector[i], replace = TRUE) + effect.size
      #my.test <- t.test(my.dist.01, my.dist.02)
      my.test <- wilcox.test(my.dist.01, my.dist.02)
      iter.vector[j] <- my.test$p.value
    }
    power.bootstrap.vector[i] <- 1 - length(which(iter.vector > 0.05))/n.iterations
  }

  # plot
  plot(x = n.vector, y = power.bootstrap.vector, type = "b", pch = 4, xlab = "Sample size (n per group)", ylab = "Power (1 - β)")
  #points(x = n.vector, y = power.bootstrap.vector, type = "b", lty = 2, pch = 1)

  # calculate sample size required to obtain power = 0.8
  f <- approxfun(x = power.bootstrap.vector, y = n.vector, method="constant")
  f(0.8)
}
