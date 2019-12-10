library(lattice)
library(latticeExtra)
library(gridExtra)
library(grid)
library(sgdnet) # branch: batch-opt

plot_batch_path <- function(x, y, alpha, family, nm) {
  source("../optB.R")

  data <- opt(x, y, family=family, alpha=alpha)
  if (min(data$B) < 100) {
    cut_off <- min(which(data$B<100))
  } else { cut_off <- length(data$B) }

  text <- paste0("\u03BB = ", round(data$lambda[cut_off], 3),
                 ", B = ", data$B[cut_off])

  t <- ifelse(alpha ==0, "ridge", "lasso")

  p <- xyplot(B ~ lambda, data=data, type='l',
    xlab = 'Lambda', ylab = 'Optimal Batch Size', main = paste0(nm," ", t),
    panel = function(...) {
      panel.xyplot(...)
      panel.abline(h = data$B[cut_off],
                   col = "red", lwd=1, lty=2)
      grid.text(text, unit(0.6, 'npc'), unit(0.2, 'npc'))
    })

  p
}

path_benchmark <- function(x, y, family, lambda, penalty) {
  data <-  data.frame(
    package = character(),
    penalty = character(),
    time = double(),
    lambda = double())

  for (j in seq_len(length(lambda))) {
    set.seed(j)
    cat("\r", j, "/", length(lambda))
    flush.console()
    if (j == length(lambda))
      cat("\n")

    alpha <- ifelse(penalty == "ridge", 0, 1)

    saga_time <- system.time({
      saga_fit <- sgdnet(
        x,
        y,
        family = family,
        lambda = lambda[j],
        alpha = alpha,
        intercept = TRUE,
        standardize = FALSE,
        batchsize = 1,
        thresh = 1e-3,
        maxit = 1e8)
    })

    batch_time <- system.time({
      batch_fit <- sgdnet(
        x,
        y,
        family = family,
        lambda = lambda[j],
        alpha = alpha,
        intercept = TRUE,
        standardize = FALSE,
        batchsize = 100,
        thresh = 1e-3,
        maxit = 1e8)
    })

    data <- rbind(data,
      data.frame(
        package = c("sgdnet (B = 1)", "sgdnet (B > 1)"),
        time = c(saga_time[3], batch_time[3]),
        penalty = penalty,
        lambda = lambda[j]
      ))
  }
  data
}

setwd("~/sgdnet-merge-work/")
load("benchmark data/abalone.rda")
x <- scale((abalone$x))
y <- abalone$y

fit <- glmnet::glmnet(x, y, alpha = 1, nlambda = 10)
p1 <- plot_batch_path(x, y, 1, "gaussian", "abalone")
data <- path_benchmark(x, y, "gaussian", fit$lambda, "lasso")
p2 <- xyplot(time ~ lambda, group = package, data = data, type = 'l', auto.key = TRUE)

fit <- glmnet::glmnet(x, y, alpha = 0, nlambda = 10)
p3 <- plot_batch_path(x, y, 0, "gaussian", "abalone")
data <- path_benchmark(x, y, "gaussian", fit$lambda, "ridge")
p4 <- xyplot(time ~ lambda, group = package, data = data, type = 'l', auto.key = TRUE)

png(filename="abalone.png", width = 16, height = 4, units = 'in', res = 300)
grid.arrange(p1, p2, p3, p4, ncol = 4)
dev.off()

####################
load("benchmark data/cadata.rda")
x <- scale((cadata$x))
y <- cadata$y

fit <- glmnet::glmnet(x, y, alpha = 1, nlambda = 10)
p1 <- plot_batch_path(x, y, 1, "gaussian", "cadata")
data <- path_benchmark(x, y, "gaussian", fit$lambda, "lasso")
p2 <- xyplot(time ~ lambda, group = package, data = data, type = 'l', auto.key = TRUE)

fit <- glmnet::glmnet(x, y, alpha = 0, nlambda = 10)
p3 <- plot_batch_path(x, y, 0, "gaussian", "cadata")
data <- path_benchmark(x, y, "gaussian", fit$lambda, "ridge")
p4 <- xyplot(time ~ lambda, group = package, data = data, type = 'l', auto.key = TRUE)

png(filename = "cadata.png", width = 16, height = 4, units = 'in', res = 300)
grid.arrange(p1, p2, p3, p4, ncol = 4)
dev.off()

####################
load("benchmark data/ijcnn1.rda")
x <- scale((ijcnn1$x))
y <- ijcnn1$y

fit <- glmnet::glmnet(x, y, alpha = 1, family = "binomial", nlambda = 10)
p1 <- plot_batch_path(x, y, 1, "binomial", "ijcnn1")
data <- path_benchmark(x, y, "binomial", fit$lambda, "lasso")
p2 <- xyplot(time ~ lambda, group = package, data = data, type = 'l', auto.key = TRUE)

fit <- glmnet::glmnet(x, y, alpha = 0, family = "binomial", nlambda = 10)
p3 <- plot_batch_path(x, y, 0, "binomial", "ijcnn1")
data <- path_benchmark(x, y, "binomial", fit$lambda, "ridge")
p4 <- xyplot(time ~ lambda, group = package, data = data, type = 'l', auto.key = TRUE)

png(filename = "ijcnn1.png", width = 16, height = 4, units = 'in', res = 300)
grid.arrange(p1, p2, p3, p4, ncol = 4)
dev.off()

####################
load("benchmark data/poker.rda")
x <- scale((poker$x))
y <- poker$y

fit <- glmnet::glmnet(x, y, alpha = 1, family = "multinomial", nlambda = 10)
p1 <- plot_batch_path(x, y, 1, "multinomial", "poker")
data <- path_benchmark(x, y, "multinomial", fit$lambda, "lasso")
p2 <- xyplot(time ~ lambda, group = package, data = data, type = 'l', auto.key = TRUE)

fit <- glmnet::glmnet(x, y, alpha = 0, family = "multinomial", nlambda = 10)
p3 <- plot_batch_path(x, y, 0, "multinomial", "poker")
data <- path_benchmark(x, y, "multinomial", fit$lambda, "ridge")
p4 <- xyplot(time ~ lambda, group = package, data = data, type = 'l', auto.key = TRUE)

png(filename = "poker.png", width = 16, height = 4, units = 'in', res = 300)
grid.arrange(p1, p2, p3, p4, ncol = 4)
dev.off()

####################
load("benchmark data/naval.rda")
x <- scale((naval$x))
y <- naval$y

fit <- glmnet::glmnet(x, y, alpha = 1, family = "mgaussian", nlambda = 10)
p1 <- plot_batch_path(x, y, 1, "mgaussian", "naval")
data <- path_benchmark(x, y, "mgaussian", fit$lambda, "lasso")
p2 <- xyplot(time ~ lambda, group = package, data = data, type = 'l', auto.key = TRUE)

fit <- glmnet::glmnet(x, y, alpha = 0, family = "mgaussian", nlambda = 10)
p3 <- plot_batch_path(x, y, 0, "mgaussian", "naval")
data <- path_benchmark(x, y, "mgaussian", fit$lambda, "ridge")
p4 <- xyplot(time ~ lambda, group = package, data = data, type = 'l', auto.key = TRUE)

png(filename = "naval.png", width = 16, height = 4, units = 'in', res = 300)
grid.arrange(p1, p2, p3, p4, ncol = 4)
dev.off()
