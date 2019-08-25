# in order to run this script #
# please get a version of sgdnet on the batch-opt branch # 
# and download optB.R to your working dir #
# and download these datasets #

#------ Helper function ------#
pass_plot <- function(datasets, data, way){
  
  library(lattice)
  library(latticeExtra)
  library(grid)
  lattice.options(default.theme = list(fontsize = list(points = 4, text = 8)))
  
  if (way == "lambda") {
    # only one dataset
    mytext = character()
    size   <- data[1,'batch']
    step   <- format(round(data[1,'step'] , 4), scientific=T)
    s_end  <- round(data[1,'stime'], 3)
    b_end  <- round(data[1,'btime'], 3)
    
    mytext <- c(mytext, paste0("\n saga: ", s_end, 
                               "(s) \n batch: ", b_end,
                               "(s) \n batch size: ", size,
                               "\n batch step: ", step))
    p <- xyplot(loss ~ pass|lambda+dataset, 
                groups = method, 
                data = data, 
                type = c("p", "l"),
                panel=function(x, y,...){
                  panel.xyplot(x,y,...)
                  grid.text(mytext[panel.number()], 
                            unit(0.75, 'npc'), 
                            unit(0.85, 'npc'))
                },
                auto.key = TRUE)
  } else {
    mytext = character()
    # same lambda, many dataset
    for (i in seq_along(datasets)) {
      
      part   <- (data['dataset'] == names(datasets)[i]) 
      part   <- data[part,]
      size   <- part[1,'batch']
      step   <- format(round(part[1,'step'] , 4), scientific=T)
      s_end  <- round(part[1,'stime'], 3)
      b_end  <- round(part[1,'btime'], 3)
      
      mytext <- c(mytext, paste0("\n saga: ", s_end, 
                                 "(s) \n batch: ", b_end,
                                 "(s) \n batch size: ", size,
                                 "\n batch step: ", step))
      
    }
    p <- xyplot(loss ~ pass|dataset, 
                groups = method, 
                data = data, 
                type = c("p", "l"),
                panel=function(x, y,...){
                  panel.xyplot(x,y,...)
                  grid.text(mytext[panel.number()], 
                            unit(0.75, 'npc'), 
                            unit(0.85, 'npc'))
                },
                auto.key = TRUE)
  }
  p
}

epochloss <- function(datasets, family, lambda) {
  source("opt.R")
  
  options(sgdnet.debug = TRUE)
  
  data <-  data.frame(dataset = character(),
                      method  = character(),
                      lambda  = double(),
                      loss  = double(),
                      pass  = integer(),
                      stime = double(),
                      btime = double(),
                      batch = integer(),
                      step  = double())
  
  for (i in seq_along(datasets)) {
    
    x <- datasets[[i]]$x
    y <- datasets[[i]]$y
    
    set.seed(i)
    
    for (m in 1:length(lambda)) {
      
      batch_info <- opt(x, y, family, lambda[m])
      opt_B <- batch_info$B
      step  <- batch_info$step
      
      saga_time <- system.time({
        saga <- sgdnet(x, y, family, 
                       standardize = FALSE, 
                       alpha = 0, lambda = lambda[m], 
                       batchsize = 1, thresh = 1e-3)
      })
      
      batch_time <- system.time({
        batch <- sgdnet(x, y, family, 
                        standardize = FALSE, 
                        alpha = 0, lambda = lambda[m], 
                        batchsize = 100, thresh = 1e-3)
      })
      
      bpass <- batch$npasses
      bloss <- attr(batch, "diagnostics")$loss[[1]]
      
      spass <- saga$npasses
      sloss <- attr(saga, "diagnostics")$loss[[1]]
      
      for (j in 1:spass) {
        data <- rbind(data,
                      data.frame( 
                        dataset = names(datasets)[i], 
                        method  = "saga",
                        lambda = toString(lambda[m]),
                        loss = sloss[j], pass = j,
                        stime = saga_time[3],
                        btime = batch_time[3],
                        batch = opt_B,
                        step = step
                      ))
      }
      
      for (k in 1:bpass) {
        data <- rbind(data,
                      data.frame(
                        dataset = names(datasets)[i], 
                        method  = "batch-opt",
                        lambda = toString(lambda[m]),
                        loss = bloss[k], pass = k,
                        stime = saga_time[3],
                        btime = batch_time[3],
                        batch = opt_B,
                        step = step
                      ))
      }
    }
  }
  data
}

#------ Benchmark ------#

# load("/home/xsslnc/benchmark/dataset/YearPredictionMSD_test.rda")
YearPredictionMSD_test$x <- scale(YearPredictionMSD_test$x)
dataset  <- list(YearPredictionMSD_test = YearPredictionMSD_test)
year_1   <- epochloss(dataset, "gaussian", 1)
year_1_p <- pass_plot(dataset, year_1, "lambda")
png(filename="year_1.png", width = 4, height = 4, units = 'in', res = 300)
year_1_p
dev.off()

# load("/home/xsslnc/benchmark/dataset/slice.rda")
# unscale
dataset <- list(slice_unscale = slice)
slice_0_1  <- epochloss(dataset, "gaussian", 0.1)
slice_0_1_p <- pass_plot(dataset, slice_0_1, "lambda")
png(filename="slice_0_1.png", width = 4, height = 4, units = 'in', res = 300)
slice_0_1_p
dev.off()

# load("/home/xsslnc/benchmark/dataset/ijcnn1_full.rda")
ijcnn1$x     <- scale(ijcnn1$x)
dataset      <- list(ijcnn1_full = ijcnn1)
ijcnn1_0_1   <- epochloss(dataset, "binomial", 0.1)
ijcnn1_0_1_p <- pass_plot(dataset, ijcnn1_0_1, "lambda")
png(filename="ijcnn1_0_1.png", width = 4, height = 4, units = 'in', res = 300)
ijcnn1_0_1_p
dev.off()

# load("/home/xsslnc/benchmark/dataset/covtype_binary_scale.rda")
dataset  <- list(covtype_binary_scale = covtype_binary_scale)
covtype_binary_scale$x <- scale(covtype_binary_scale$x)
covtype_0_1   <- epochloss(dataset, "binomial", 0.1)
covtype_0_1_p <- pass_plot(dataset, covtype_0_1, "lambda")
png(filename="covtype_0_1.png", width = 4, height = 4, units = 'in', res = 300)
covtype_0_1_p
dev.off()

