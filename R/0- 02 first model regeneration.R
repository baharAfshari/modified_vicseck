source("R/utils_model0.R")

library(dplyr)
library(ggplot2)


mean_gaussian <- 100
sd_gaussian <- 5
etha <- 0.4


n_agents <- 1000
v0_value <- 1
time <- 1000
ensemble <- 10


ensembles <- list()
for (ens in 1:ensemble) {
  
  v <- list()
  v_x <- runif(n_agents, min = -1, max = 1)
  v_y <- runif(n_agents, min = -1, max = 1)
  v[[1]] <- list(v_x = v_x, v_y = v_y)
  v[[1]][["size"]] <- v_size(v[[1]]$v_x, v[[1]]$v_y)
  v[[1]][["x_normal"]] <- v[[1]][["v_x"]]/v[[1]][["size"]]
  v[[1]][["y_normal"]] <- v[[1]][["v_y"]]/v[[1]][["size"]]
  
  gaussian_values <- floor(abs(rnorm(2500, mean = mean_gaussian, sd = sd_gaussian)))
  relations <- matrix(gaussian_values, nrow = n_agents, ncol = n_agents)
  diag(relations) <- 0
  
  
  for(t in 2:time){
    new_vx <- vector(length = n_agents)
    new_vy <- vector(length = n_agents)
    new_v_size <- vector(length = n_agents)
    new_v_x_normal <- vector(length = n_agents)
    new_v_y_normal <- vector(length = n_agents)
    for (i in 1:n_agents) {
      new_v <- sum_neighbors(v, relations, t, i,v0_value)
      new_vx[i] = new_v$v_x
      new_vy[i] = new_v$v_y
      new_v_size[i] = new_v$size
      new_v_x_normal[i] = new_v$x_normal
      new_v_y_normal[i] = new_v$y_normal
    }
    v[[t]] <- list(v_x = new_vx, v_y = new_vy, size = new_v_size, x_normal = new_v_x_normal, y_normal = new_v_y_normal)
  }
  
  order_param <- sapply(v, order_parameter)
  ensembles[[ens]] <- list(velocities = v, order_param = order_param)
}

order_overall_list <- list()
order_overall <- matrix(0, nrow = time, ncol = ensemble)
for (i in 1:ensemble) {
  order_overall_list[[i]] <- ensembles[[i]]$order_param
  order_overall[, i] <- ensembles[[i]]$order_param
}

order_overall <- order_overall %>% as.data.frame() %>% mutate(overall = rowMeans(.))

titles <- paste0("ethe: " ,etha, ", mean gaussian: ", mean_gaussian, ", standard deviation gaussian: ", sd_gaussian)
ggplot(order_overall, aes(1:1000, overall)) +
  geom_line() + labs(title = titles)



