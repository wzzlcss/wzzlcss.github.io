# in order to run this script #
# please get a version of sgdnet on the batch-opt branch # 
# and download optB.R to your working dir #

#------ Helper function ------#
library(lattice)
library(latticeExtra)
library(grid)
lattice.options(default.theme = list(fontsize = list(points = 4, text = 8)))

complexity_plot <- function(dataset, data, text) {
  
  p <- xyplot(epoch ~ batch|dataset+lambda,
              data = data,
              type = "l",
              panel = function(...) {
                panel.xyplot(...)
                panel.abline(v = data[1,'optB'],
                             col="red", lwd=1, lty=2)
                grid.text(text, 
                          unit(0.3, 'npc'), 
                          unit(0.9, 'npc'))
              },
              auto.key = TRUE)
  p
}

opt_step_complexity <- function(datasets, family, alpha, lambda, batch_max, batch_freq) {
  
  # enable step size input in source code first
  
  # specify the working dir
  source("optB.R")
  
  # one dataset
  x <- datasets[[1]]$x
  y <- datasets[[1]]$y
  
  batch_seq <- seq(from = 1, to = batch_max, by = batch_freq)
  
  batch_info <- opt(x, y, family, lambda)
  step <- batch_info$step
  opt_B <- batch_info$B
  
  data <- data.frame(dataset = character(),
                     lambda  = double(),
                     npass = integer(),
                     batch = integer(),
                     optB = integer())
  
  # increase to full batch
  for (i in 1:length(batch_seq)) {
    
    saga <- sgdnet(x, y, family,
                   standardize = FALSE,
                   alpha = alpha, lambda = lambda,
                   batchsize = batch_seq[i],
                   stepsize = step,
                   maxit = 1e3)
    
    data <- rbind(data,
                  data.frame(
                    dataset = names(datasets)[1],
                    lambda = toString(lambda),
                    epoch = saga$npasses,
                    batch = batch_seq[i],
                    optB = opt_B
                  ))
  }
  data
}

complexity <- function(datasets, family, alpha, lambda, batch_max, batch_freq) {
  
  # specify the working dir
  source("optB.R")
  
  # one dataset
  x <- datasets[[1]]$x
  y <- datasets[[1]]$y
  
  batch_seq <- seq(from = 1, to = batch_max, by = batch_freq)
  
  batch_info <- opt(x, y, family, lambda)
  opt_B <- batch_info$B
  
  data <- data.frame(dataset = character(),
                     lambda  = double(),
                     npass = integer(),
                     batch = integer(),
                     optB = integer())
  
  # increase batch
  for (i in 1:length(batch_seq)) {
    
    saga <- sgdnet(x, y, family,
                   standardize = FALSE,
                   alpha = alpha, lambda = lambda,
                   batchsize = batch_seq[i],
                   maxit = 1e3)
    
    data <- rbind(data,
                  data.frame(
                    dataset = names(datasets)[1],
                    lambda = toString(lambda),
                    epoch = saga$npasses,
                    batch = batch_seq[i],
                    optB = opt_B
                  ))
  }
  data
}

#------ Benchmark ------#
data("abalone")
abalone$x <- scale(abalone$x)
dataset  <- list(abalone = abalone)
data <- complexity(dataset, "gaussian", 0, 10, 1000, 10)
p <- complexity_plot(dataset, data, "Defazio step size")
png(filename="abalone_com.png", width = 3, height = 3, units = 'in', res = 300)
p
dev.off()

data <- opt_step_complexity(dataset, "gaussian", 0, 10, 1000, 10)
p <- complexity_plot(dataset, data, "optimal step size")
png(filename="abalone_com_step_opt.png", width = 3, height = 3, units = 'in', res = 300)
p
dev.off()

library(glmnet)
x <- scale(abalone$x)
y <- abalone$y
fit <- glmnet(x, y, alpha = 0)
lambda_seq <- fit$lambda[50:100]
B <- step <- rep(0, length(lambda_seq))

for (i in 1:length(lambda_seq)) {
  batch_info <- opt(x, y, "gaussian", lambda_seq[i])
  B[i] <- batch_info$B
  step[i] <- batch_info$step
}

data_batch <- list(optimal = B, 
                   lambda = lambda_seq,
                   grp = rep("optimal batch", length(lambda_seq)))

data_step  <- list(optimal = step, 
                   lambda = lambda_seq,
                   grp = rep("optimal step size", length(lambda_seq)))

p1 <- xyplot(optimal~ lambda|grp,
             data = data_batch,
             type = "l")

p2 <- xyplot(optimal~ lambda|grp,
             data = data_step,
             type = "l")

png(filename="abalone_batch.png", width = 6, height = 2, units = 'in', res = 300)
p1
dev.off()

png(filename="abalone_step.png", width = 6, height = 2, units = 'in', res = 300)
p2
dev.off()