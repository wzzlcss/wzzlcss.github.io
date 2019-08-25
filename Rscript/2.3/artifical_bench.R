# in order to run this script #
# please get a version of sgdnet on the dev branch # 

#------ Helper function ------#
artifical_plot <- function(dataset, data, alpha) {
  
  library(lattice)
  library(latticeExtra)
  library(grid)
  lattice.options(default.theme = list(fontsize = list(points = 4, text = 8)))
  
  mytext = character()
  for (i in 1:length(alpha)){
    penalty <- ifelse(alpha[i] == 0, "ridge", "lasso")
    data_part <- (data['penalty'] == penalty)
    data_part <- data[data_part,]
    saga <- (data_part['method'] == "saga")
    saga <- data_part[saga,]
    spass <- max(saga['pass'])
    stime <- round(saga[1, 'time'], 3)
    
    batch <- (data_part['method'] == "batch")
    batch <- data_part[batch,]
    bpass <- max(batch['pass'])
    btime <- round(batch[1, 'time'],3)
    
    cyclic <- (data_part['method'] == "cyclic")
    cyclic <- data_part[cyclic,]
    cpass <- max(cyclic['pass'])
    ctime <- round(cyclic[1, 'time'],3)
    
    mytext <- c(mytext, paste0("\n saga: ", stime, "(s), ", spass, " epoch",
                               "\n batch: ", btime, "(s), ", bpass, " epoch",
                               "\n cyclic: ", ctime, "(s), ", cpass, " epoch"))
  }
  
  p <- xyplot(loss ~ pass|dataset+penalty,
              groups = method,
              data = data,
              type = c("p", "l"),
              panel=function(x, y,...){
                panel.xyplot(x,y,...)
                grid.text(mytext[panel.number()],
                          unit(0.55, 'npc'),
                          unit(0.83, 'npc'))
              },
              auto.key = TRUE)
  p
}

artificial <- function(datasets, family, alpha) {
  
  options(sgdnet.debug = TRUE)
  
  data <-  data.frame(dataset = character(),
                      method  = character(),
                      penalty = character(),
                      loss  = double(),
                      pass  = integer(),
                      time = double())
  
  # one artifical dataset
  x <- datasets[[1]]$x
  y <- datasets[[1]]$y
  
  n <- nrow(x)
  
  for (i in 1:length(alpha)) {
    set.seed(i)
    saga_time <- system.time({
      saga <- sgdnet(x, y, family,
                     standardize = FALSE,
                     alpha = alpha[i], lambda = 1/n,
                     batchsize = 1)
    })
    
    batch_time <- system.time({
      batch <- sgdnet(x, y, family,
                      standardize = FALSE,
                      alpha = alpha[i], lambda = 1/n,
                      batchsize = 10)
    })
    
    cyclic_time <- system.time({
      cyclic <- sgdnet(x, y, family,
                       standardize = FALSE,
                       alpha = alpha[i], lambda = 1/n,
                       cyclic = TRUE)
    })
    
    spass <- saga$npasses
    sloss <- attr(saga, "diagnostics")$loss[[1]]
    
    bpass <- batch$npasses
    bloss <- attr(batch, "diagnostics")$loss[[1]]
    
    cpass <- cyclic$npasses
    closs <- attr(cyclic, "diagnostics")$loss[[1]]
    
    penalty <- ifelse(alpha[i] == 0, "ridge", "lasso")
    
    for (j in 1:spass) {
      data <- rbind(data,
                    data.frame(
                      dataset = names(datasets)[1],
                      method  = "saga",
                      penalty = penalty,
                      loss = sloss[j], pass = j,
                      time = saga_time[3]
                    ))
    }
    
    for (k in 1:bpass) {
      data <- rbind(data,
                    data.frame(
                      dataset = names(datasets)[1],
                      method  = "batch",
                      penalty = penalty,
                      loss = bloss[k], pass = k,
                      time = batch_time[3]
                    ))
    }
    
    for (m in 1:cpass) {
      data <- rbind(data,
                    data.frame(
                      dataset = names(datasets)[1],
                      method  = "cyclic",
                      penalty = penalty,
                      loss = closs[m], pass = m,
                      time = cyclic_time[3]
                    ))
    }
  }
  data
}
#------ Benchmark ------#
set.seed(1)
x <- scale(matrix(rnorm(100*10),100,10))
y1 <- scale(rnorm(100))
gaussian <- list(x = x, y = y1)
dataset  <- list(gaussian = gaussian)
data <- artificial(dataset, "gaussian", c(0,1))
p <- artifical_plot(dataset, data, c(0,1))
png(filename="gaussian_art.png", width = 3, height = 4, units = 'in', res = 300)
p
dev.off()

y2 <- sample(1:2, 100, replace=TRUE)
binomial <- list(x = x, y = y2)
dataset  <- list(binomial = binomial)
data <- artificial(dataset, "binomial", c(0,1))
p <- artifical_plot(dataset, data, c(0,1))
png(filename="binomial_art.png", width = 3, height = 4, units = 'in', res = 300)
p
dev.off()

y3 <- scale(matrix(rnorm(100*3),100,3))
mgaussian <- list(x = x, y = y3)
dataset  <- list(mgaussian = mgaussian)
data <- artificial(dataset, "mgaussian", c(0,1))
p <- artifical_plot(dataset, data, c(0,1))
png(filename = "mgaussian_art.png", width = 3, height = 4, units = 'in', res = 300)
p
dev.off()

y4 <- sample(1:4, 100, replace=TRUE)
multinomial <- list(x = x, y = y4)
dataset  <- list(multinomial = multinomial)
data <- artificial(dataset, "multinomial", c(0,1))
p <- artifical_plot(dataset, data, c(0,1))
png(filename = "multinomial_art.png", width = 3, height = 4, units = 'in', res = 300)
p
dev.off()
