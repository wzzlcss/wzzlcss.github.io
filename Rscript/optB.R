mu <- function(x, n_sample, n_feature) {
  if (n_feature < n_sample)
    mu <- min(eigen(t(x)%*%x)$values)/n_sample
  else
    mu <- min(eigen(x%*%t(x))$values)/n_sample
  return(mu)
}

Lmax <- function(x, n_sample) {
  colnorm <- rep(0, n_sample)
  for (i in 1:n_sample) {
    colnorm[i] <- sum(x[i,]^2)
  }
  max <- max(colnorm)
  return(max)
}

L <- function(x, n_sample, n_feature) {
  if (n_feature < n_sample)
    l = max(eigen(t(x)%*%x)$values)/n_sample
  else
    l = max(eigen(x%*%t(x))$values)/n_sample
  return(l)
}

Lbar <- function(x, n_sample) {
  colnorm <- rep(0, n_sample)
  for (i in 1:n_sample) {
    colnorm[i] <- sum(x[i,]^2)
  }
  return(mean(colnorm))
}

Lpratical <- function(B, n, L, Lmax, lambda) {
  L*n*(B-1)/(B*(n-1)) + Lmax*(n-B)/(B*(n-1)) + lambda
}

rightterm <- function(B, n, mu, Lmax, lambda) {
  (Lmax + lambda)*(n-B)/(B*(n-1)) + (mu + lambda)*n/(4*B)
}

stepsize <- function(Lpratical, rightterm) {
  1/(4*max(Lpratical, rightterm))
}

opt <- function(x, y, family, lambda, alpha) {

  if (missing(lambda)) {
     fit <- glmnet::glmnet(x, y, family = family, alpha = alpha)
     lambda = fit$lambda
  }
  lambda <- as.vector(lambda)
  n <- length(lambda)
  n_sample <- nrow(x)
  n_feature <-ncol(x)
  B <- step <- max <- bar <- mu <- L <- rep(0,n)

  for (i in 1:n) {
    x_mu <- mu(x, n_sample, n_feature)
    x_Lmax <- Lmax(x, n_sample)
    x_L <- L(x, n_sample, n_feature)
    x_Lbar <- Lbar(x, n_sample)
    max[i] <- x_Lmax
    bar[i] <- x_Lbar
    mu[i] <- x_mu
    L[i] <- x_L
    B[i] <- floor(1 + ((x_mu + lambda[i]) * (n_sample-1))/(4 * (x_L + lambda[i])))
    pra <- Lpratical(B[i], n_sample, x_L, x_Lmax, lambda[i])
    right <- rightterm(B[i], n_sample, x_mu, x_Lmax, lambda[i])
    step[i] <- stepsize(pra, right)
  }
  list(B=B, step=step, lambda=lambda, max=max, bar=bar, mu=mu, L=L)
}



